package rio

import spinal.core._
import spinal.lib._

// flow = vPIFOs + FlowId

case class EngineConfig(
    numEngines: Int,
    numVPIFOs: Int,
    maxPacketPriority: Int,
    fifoDepth: Int,
    prefetchBufferDepth: Int,
    brainStateWidth: Int = 32,
    flowStateWidth: Int = 32,
    configDataWidth: Int = 32,
    commitQueueLength: Int = EngineConfig.DefaultCommitQueueLength,
    pifoBackend: String = "house",
    dynamicConfig: Boolean = true,
    lookupBackend: String = EngineConfig.DefaultLookupBackend,
    camEntriesPerPe: Int = 0,
    camEntriesPerStage: Int = 128
) {
  require(Set("house", "external").contains(pifoBackend))
  require(Set("dense", "cam", "cam-pipelined").contains(lookupBackend))
  require(camEntriesPerPe >= 0)
  require(camEntriesPerStage >= 2 && isPow2(camEntriesPerStage))
  def usesCam = lookupBackend != "dense"
  def pipelinedCam = lookupBackend == "cam-pipelined"
  def camReadLatency = (camCapacity + camEntriesPerStage - 1) / camEntriesPerStage + 1
  def lookupBufferDepth = camReadLatency + 3
  require(commitQueueLength >= 2 && (commitQueueLength & (commitQueueLength - 1)) == 0,
    "commitQueueLength must be a power of two >= 2")
  def vpifoIdWidth = log2Up(numVPIFOs)
  def numFlows = numVPIFOs * numEngines
  def engineIdWidth = log2Up(numEngines + 1) // +1 for control port
  def flowIdWidth = vpifoIdWidth + engineIdWidth
  // Space for two tree contexts per flow by default; callers can set a larger
  // bound when more distinct (vPIFO, flow token) pairs coexist.
  def camCapacity = if (camEntriesPerPe == 0) 2 * numVPIFOs else camEntriesPerPe

  def dequePredWidth = vpifoIdWidth + 1 // +1 for exist bit

  def numBrainState = 1 << brainStateWidth
  def numFlowState = 1 << flowStateWidth

  assert(flowStateWidth <= configDataWidth, "flowStateWidth should be less than configDataWidth")
  assert(brainStateWidth % configDataWidth == 0, "brainStateWidth should be multiple of configDataWidth")
}

object EngineConfig {
  val DefaultCommitQueueLength = 256
  val DefaultLookupBackend = "cam"

  implicit def toFlowPifoConfig(pifoConfig: EngineConfig): PifoConfig =
    PifoConfig(
      numPifo = pifoConfig.numVPIFOs * pifoConfig.fifoDepth,
      bitPort = pifoConfig.vpifoIdWidth,
      bitPrio = log2Up(pifoConfig.maxPacketPriority),
      bitData = pifoConfig.flowIdWidth
    )
}

case class MapperUpdater(inputWidth: Int, outputWidth: Int) extends Bundle {
  val inputId = UInt(inputWidth bits)
  val outputId = UInt(outputWidth bits)
}

// TODO(zhiyuang): check for the writeFirst policy
case class Mapper(inputWidth: Int, outputWidth: Int) extends Component {
  val numInputs = 1 << inputWidth
  def updater = MapperUpdater(inputWidth, outputWidth)

  val io = new Bundle {
    val readReq = slave Flow (UInt(inputWidth bits))
    val readRes = master Flow (UInt(outputWidth bits))

    val writeReq = slave Flow (updater)
  }

  val ram = MapperMemory.zeroed(outputWidth, numInputs)

  // read logic
  ram.flowReadSync(io.readReq) >> io.readRes

  // write logic
  ram.writePort().translateFrom(io.writeReq) { (to, from) =>
    to.address := from.inputId
    to.data := from.outputId
  }
}

/**
  * A mapper whose control-plane writes become packet-visible atomically.
  *
  * Packet reads use the active bank while updates write the backup bank. A commit
  * swaps the banks in one cycle, then the new active bank is copied back before
  * another update or commit is accepted.
  */
case class TransactionalMapper(inputWidth: Int, outputWidth: Int) extends Component {
  require(inputWidth > 0, "inputWidth must be positive")
  require(outputWidth > 0, "outputWidth must be positive")

  val numInputs = 1 << inputWidth
  def updater = MapperUpdater(inputWidth, outputWidth)

  val io = new Bundle {
    val readReq = slave Flow (UInt(inputWidth bits))
    val readRes = master Flow (UInt(outputWidth bits))

    val writeReq = slave Stream (updater)
    val commit = in Bool ()
    val commitReady = out Bool ()
  }

  val banks = Seq.fill(2)(MapperMemory.zeroed(outputWidth, numInputs))
  val activeBank = RegInit(False)
  val copying = RegInit(False)
  val copyAddress = Reg(UInt(inputWidth bits)) init (0)
  val copyWriteValid = RegNext(copying) init (False)
  val copyWriteAddress = RegNext(copyAddress) init (0)
  val synchronizing = copying || copyWriteValid

  // Read both banks so a request issued on the commit cycle still returns data
  // from the bank that was active for that request.
  val bankRead = Seq(
    banks(0).readSync(io.readReq.payload, io.readReq.valid && !activeBank),
    banks(1).readSync(io.readReq.payload, io.readReq.valid && activeBank)
  )
  val requestedBank = Reg(Bool()) init (False)
  when(io.readReq.valid) {
    requestedBank := activeBank
  }
  io.readRes.valid := RegNext(io.readReq.valid) init (False)
  io.readRes.payload := Mux(requestedBank, bankRead(1), bankRead(0))

  io.writeReq.ready := !synchronizing
  io.commitReady := !synchronizing

  // A second synchronous read port walks the active bank during synchronization.
  // Its one-cycle-delayed result uses the backup bank's normal write port, while
  // packet reads continue through the first port.
  val copyRead = Seq(
    banks(0).readSync(copyAddress, copying && !activeBank),
    banks(1).readSync(copyAddress, copying && activeBank)
  )
  val copyData = Mux(
    activeBank,
    copyRead(1),
    copyRead(0)
  )
  val writeAddress = Mux(copyWriteValid, copyWriteAddress, io.writeReq.payload.inputId)
  val writeData = Mux(copyWriteValid, copyData, io.writeReq.payload.outputId)
  val writeEnable = copyWriteValid || io.writeReq.fire

  banks(0).write(writeAddress, writeData, writeEnable && activeBank)
  banks(1).write(writeAddress, writeData, writeEnable && !activeBank)

  when(io.commit && io.commitReady) {
    activeBank := !activeBank
    copying := True
    copyAddress := 0
  } elsewhen (copying) {
    when(copyAddress === U(numInputs - 1, inputWidth bits)) {
      copying := False
    } otherwise {
      copyAddress := copyAddress + 1
    }
  }
}

/** A single-bank front-end rewrite table activated by a drained PIFO port.
  *
  * Control writes install source -> target with rewriting disabled. A mapper
  * commit arms newly installed entries, and the successful pop of the source's
  * final entry enables its rewrite. The table is intentionally not copied or
  * banked: its runtime enable bit is data-plane state rather than transactional
  * mapper state.
  */
case class FrontRewriteTable(pifoIdWidth: Int) extends Component {
  require(pifoIdWidth > 0, "pifoIdWidth must be positive")

  val numPifos = 1 << pifoIdWidth

  val io = new Bundle {
    val lookupSource = in UInt (pifoIdWidth bits)
    val lookupTarget = out UInt (pifoIdWidth bits)
    val lookupEnabled = out Bool ()
    val lookupCanEnable = out Bool ()

    val drained = slave Flow (UInt(pifoIdWidth bits))
    val drainEnablesRewrite = out Bool ()
    val emptySource = slave Flow (UInt(pifoIdWidth bits))
    val emptyEnablesRewrite = out Bool ()

    val writeReq = slave Stream (MapperUpdater(pifoIdWidth, pifoIdWidth))
    val commit = in Bool ()
  }

  val targets = Vec.fill(numPifos)(Reg(UInt(pifoIdWidth bits)) init (0))
  val configured = Vec.fill(numPifos)(Reg(Bool()) init (False))
  val pending = Vec.fill(numPifos)(Reg(Bool()) init (False))
  val armed = Vec.fill(numPifos)(Reg(Bool()) init (False))
  val enabled = Vec.fill(numPifos)(Reg(Bool()) init (False))

  val drainedEntryEligible =
    armed(io.drained.payload) || (io.commit && pending(io.drained.payload))
  io.drainEnablesRewrite :=
    io.drained.valid && configured(io.drained.payload) && drainedEntryEligible

  io.lookupTarget := targets(io.lookupSource)
  io.lookupEnabled := enabled(io.lookupSource)
  io.lookupCanEnable :=
    configured(io.lookupSource) && !enabled(io.lookupSource) && (
      armed(io.lookupSource) || (io.commit && pending(io.lookupSource))
    )

  val emptyEntryEligible =
    armed(io.emptySource.payload) || (io.commit && pending(io.emptySource.payload))
  io.emptyEnablesRewrite :=
    io.emptySource.valid && configured(io.emptySource.payload) && emptyEntryEligible

  io.writeReq.ready := True

  // The final successful source pop enables the entry for all subsequent
  // requests. This can coincide with the commit that arms the entry.
  when(io.drainEnablesRewrite) {
    enabled(io.drained.payload) := True
  }

  // If an entry was already empty when it became armed, suppress its first
  // would-underflow request and enable the rewrite directly.
  when(io.emptyEnablesRewrite) {
    enabled(io.emptySource.payload) := True
  }

  // Mapper publication arms entries written since the preceding commit. The
  // target itself is single-bank and was already installed by the control write.
  when(io.commit) {
    for (index <- 0 until numPifos) {
      when(pending(index)) {
        pending(index) := False
        armed(index) := True
      }
    }
  }

  // Give a new control write priority over a coincident old drain/commit for
  // the same source. Reprogramming starts a fresh disabled, pending entry.
  when(io.writeReq.fire) {
    val source = io.writeReq.payload.inputId
    targets(source) := io.writeReq.payload.outputId
    configured(source) := True
    pending(source) := True
    armed(source) := False
    enabled(source) := False
  }
}

case class BrainInput(config: EngineConfig) extends Bundle {
  val vpifoId = UInt(config.vpifoIdWidth bits)
  val flowId = UInt(config.flowIdWidth bits)
}

case class PIFOBrain(config: EngineConfig) extends Component {
  val io = new Bundle {
    val request = slave Stream (BrainInput(config))
    val response = master Stream (PifoEntry(config))

    val control = slave Stream (ControlMessage(config))
    val poped = slave Flow (PifoPopResponse(config))
    val camCapacityBlocked = if (config.usesCam) out Bool() else null
  }

  // Keep a queue's dependent rank updates ordered through a multi-cycle CAM
  // write. Other vPIFOs may overlap; release only after all output/update sinks
  // acknowledge, including completion of the flow-state CAM write.
  val rankBusy = if (config.pipelinedCam) Vec.fill(config.numVPIFOs)(RegInit(False)) else null
  val admitted = if (config.pipelinedCam) io.request.haltWhen(rankBusy(io.request.vpifoId)) else io.request
  if (config.pipelinedCam) when(admitted.fire) { rankBusy(admitted.vpifoId) := True }
  val inHeads = StreamFork(admitted, 5)
  val controller = new ControllerFactory(config)

  val engineMapper = Mapper(config.vpifoIdWidth, log2Up(BrainType.elements.size))
  controller.dispatch(
    ControlCommand.UpdateBrainEngine,
    engineMapper.io.writeReq
  ) { (to, from) =>
    to.inputId := from.vPifoId
    to.outputId := from.data.resized
  }

  val lastVirtualMapper = Mapper(config.vpifoIdWidth, config.bitPrio)
  lastVirtualMapper.io.writeReq.translateFrom(io.poped.throwWhen(!io.poped.exist)) { (to, from) =>
    to.inputId := from.port
    to.outputId := from.priority
  }
  val engineCAM = MeshCamLookup(config.flowIdWidth + config.vpifoIdWidth,
    config.flowStateWidth, config, replay = false)
  engineCAM.io.commit := False
  if (config.usesCam) io.camCapacityBlocked := engineCAM.io.capacityBlocked
  inHeads(2).map { data =>
    data.vpifoId @@ data.flowId
  } >> engineCAM.io.readReq
  // TODO(zhiyuang): check this priority and flows when updating
  val flowStateControl, flowStateUpdate = Stream(engineCAM.updater)
  engineCAM.io.writeReq << StreamArbiterFactory.lowerFirst.transactionLock
    .onArgs(flowStateControl, if (config.pipelinedCam) flowStateUpdate else flowStateUpdate.queueLowLatency(2))
  controller.dispatchStream(
    ControlCommand.UpdateBrainFlowState,
    flowStateControl
  ) { (to, from) =>
    to.inputId := from.vPifoId @@ from.flowId
    to.outputId := from.data.resized
  }

  val brainStateMem = Mapper(config.vpifoIdWidth, config.brainStateWidth)
  val brainStateControl, brainStateUpdate = Stream(brainStateMem.updater)
  brainStateMem.io.writeReq << StreamArbiterFactory.lowerFirst.transactionLock
    .onArgs(brainStateControl, if (config.pipelinedCam) brainStateUpdate else brainStateUpdate.queueLowLatency(2))
    .toFlow
  controller.dispatchStream(
    ControlCommand.UpdateBrainState,
    brainStateControl
  ) { (to, from) =>
    to.inputId := from.vPifoId
    to.outputId := from.data.resized
  }

  controller.build(io.control)

  def tableRead(input: Stream[UInt], mapper: Mapper): Stream[UInt] = {
    if (config.pipelinedCam) BufferedMapperRead(input, mapper.io.readReq,
      mapper.io.readRes, config.lookupBufferDepth)
    else {
      mapper.io.readReq << input.toFlow
      mapper.io.readRes.toStream.queueLowLatency(2)
    }
  }
  val engineFifo = tableRead(inHeads(0).map(_.vpifoId), engineMapper)
  val brainStateFifo = tableRead(inHeads(3).map(_.vpifoId), brainStateMem)
  val flowStateFifo = if (config.pipelinedCam) engineCAM.io.readRes else engineCAM.io.readRes.queueLowLatency(2)
  val lastVirtualFifo = tableRead(inHeads(1).map(_.vpifoId), lastVirtualMapper)
  val inputFifo = inHeads(4).queueLowLatency(if (config.pipelinedCam) config.lookupBufferDepth else 2)

  val engineStream = StreamJoin(
    Seq(
      engineFifo,
      brainStateFifo,
      flowStateFifo,
      lastVirtualFifo,
      inputFifo
    )
  ).map { data =>
    val anno = new Bundle {
      val pifoId = cloneOf(inHeads(4).payload.vpifoId)
      val flowId = cloneOf(inHeads(4).payload.flowId)
      val engineId = cloneOf(engineMapper.io.readRes.payload)
      val flowState = cloneOf(engineCAM.io.readRes.payload)
      val brainState = cloneOf(brainStateMem.io.readRes.payload)
      val virutalTime = cloneOf(lastVirtualMapper.io.readRes.payload)
    }

    anno.pifoId := inputFifo.payload.vpifoId
    anno.flowId := inputFifo.payload.flowId
    anno.engineId := engineFifo.payload
    anno.flowState := flowStateFifo.payload
    anno.brainState := brainStateFifo.payload
    anno.virutalTime := lastVirtualFifo.payload

    anno
  }

  // TODO(zhiyaung): add update logic for different brain types
  // Engine Logic
  val outStream = engineStream.map { data =>
    val res = new Bundle {
      val entry = cloneOf(io.response.payload)
      val flowUpdate = new Bundle {
        val flow = engineCAM.updater
        val update = Bool()
      }
      val brainUpdate = new Bundle {
        val brain = brainStateMem.updater
        val update = Bool()
      }
    }

    // set data to data.id
    res.entry.port := data.pifoId
    res.entry.priority := 0
    res.entry.data := data.flowId
    res.flowUpdate.flow.inputId := data.pifoId @@ data.flowId
    res.flowUpdate.flow.outputId := 0
    res.flowUpdate.update := False
    res.brainUpdate.brain.inputId := data.pifoId
    res.brainUpdate.brain.outputId := 0
    res.brainUpdate.update := False

    val brainType = BrainType()
    brainType.assignFromBits(data.engineId.asBits.resized)

    switch(brainType) {
      // strict priority
      is(BrainType.SP) {
        res.entry.priority := data.flowState.resized
      }

      // WFQ
      // Currently this works like a Round-Robin: as its weight is same for all flows
      // TODO(zhiyuang): need some assertation on weight configuration: we need to make sure it fits into the bits
      is(BrainType.WFQ) {
        val virtualTime = data.virutalTime.resize(config.bitPrio bits)
        val lastFinish = data.flowState.resize(config.bitPrio bits)

        val newStart = Mux(virtualTime > lastFinish, virtualTime, lastFinish)

        val newTime = newStart + U(16, config.bitPrio bits)
        // TODO(zhiyuang): weight handling in per-flow state
        res.entry.priority := newTime

        res.flowUpdate.update := True
        res.flowUpdate.flow.outputId := newTime.resized
      }

      // FIFO
      is(BrainType.FIFO) {
        val current = data.brainState.resize(config.bitPrio bits)
        val newPriority = current + 1

        res.entry.priority := newPriority
        res.brainUpdate.update := True
        res.brainUpdate.brain.outputId := newPriority.resized
      }
    }

    res
  }

  val (output, flowUpdates, brainUpdates) = StreamFork3(outStream)
  if (config.pipelinedCam) when(outStream.fire) { rankBusy(outStream.entry.port) := False }

  io.response << output
    .throwWhen(output.entry.priority === 0)
    .map(_.entry)

  flowStateUpdate << flowUpdates
    .throwWhen(!flowUpdates.payload.flowUpdate.update)
    .map(_.flowUpdate.flow)

  brainStateUpdate << brainUpdates
    .throwWhen(!brainUpdates.payload.brainUpdate.update)
    .map(_.brainUpdate.brain)
}

case class PifoMessage(config: EngineConfig) extends Bundle {
  val engineId = UInt(config.engineIdWidth bits)
  val vPifoId = UInt(config.vpifoIdWidth bits)

  def flowId: UInt = engineId @@ vPifoId
  def fromFlowId(id: UInt) = {
    engineId := id(config.flowIdWidth - 1 downto config.vpifoIdWidth)
    vPifoId := id(config.vpifoIdWidth - 1 downto 0)
  }
}

object PifoMessage {
  def fromData(config: EngineConfig, data: UInt, exist: Bool): PifoMessage = {
    val msg = PifoMessage(config)
    // If not exist, set engineId to 0
    msg.engineId := Mux(exist, data(config.flowIdWidth - 1 downto config.vpifoIdWidth), U(0))
    msg.vPifoId := data(config.vpifoIdWidth - 1 downto 0)
    msg
  }
}

case class PifoEngine(config: EngineConfig) extends Component {
  val io = new Bundle {
    val enqueRequest = slave Stream (PifoMessage(config))
    val dequeueRequest = slave Stream (PifoMessage(config))

    val dequeueResponse = master Stream (PifoMessage(config))

    // control signals
    val control = slave Stream (ControlMessage(config))
    val commitReady = out Bool ()
    val pifo = if (config.pifoBackend == "external") master(PifoBoundary(config)) else null
    val nearlyDrained = master Flow (UInt(config.vpifoIdWidth bits))
    val pushed = Vec(master(Flow(UInt(config.vpifoIdWidth bits))), 2)
    val camCapacityBlocked = if (config.usesCam) out Bool() else null
  }

  // PIFO
  val pifos: PifoCore = if (config.pifoBackend == "external") new ExternalPifoCore(config)
                       else new ConcurrentPifoRTL(config)
  if (config.pifoBackend == "external") io.pifo <> pifos.asInstanceOf[ExternalPifoCore].boundary
  io.nearlyDrained << pifos.io.portDrained
  (io.pushed zip pifos.io.portPushed).foreach { case (out, in) => out << in }

  // enque logic
  // enqueMapper maps flowIds to VPIFO ids
  val enque = new Area {
    val (mapperRead, flowIdStream) = StreamFork2(io.enqueRequest)

    val enqueMapper: ConfigurableMapper = if (config.dynamicConfig)
      ReplayMapper(config.vpifoIdWidth, config.vpifoIdWidth)
    else DirectMapper(config.vpifoIdWidth, config.vpifoIdWidth)
    val mapped = if (config.pipelinedCam) BufferedMapperRead(mapperRead.map(_.vPifoId),
      enqueMapper.io.readReq, enqueMapper.io.readRes, 4)
    else {
      enqueMapper.io.readReq << mapperRead.map(_.vPifoId).toFlow
      enqueMapper.io.readRes.toStream
    }

    val brainInput = Stream(BrainInput(config))
    StreamJoin(mapped, flowIdStream.queueLowLatency(if (config.pipelinedCam) 4 else 2))
      .translateInto(brainInput) { (to, from) =>
        to.vpifoId := from._1
        to.flowId := from._2.flowId
      }

    // brain takes (vpid, flowid) to PIFOEntry(priority, flowid)
    // each VPIFO has its own brain
    val brain = PIFOBrain(config)
    brain.io.request << brainInput

    // flow PIFO will give the result
    pifos.io.push1 << brain.io.response.toFlow
    // currently we do not use push2
    pifos.io.push2.valid := False
    pifos.io.push2.payload.assignDontCare()
  }

  val deque = new Area {
    // Qualify a dequeue rewrite with the PIFO port. This lets old and new
    // copies of a tree carry the same flow IDs while retaining distinct next
    // hops during a full-transitive reconfiguration.
    val dequeMapper = MeshCamLookup(config.vpifoIdWidth + config.flowIdWidth,
      config.flowIdWidth, config, replay = config.dynamicConfig)
    val frontRewrite = FrontRewriteTable(config.vpifoIdWidth)

    // Rewrites happen before the PIFO lookup. The last successful source pop
    // enables its entry. The activating cycle backpressures this PE once so the
    // waiting request observes the registered enable on the following cycle.
    frontRewrite.io.lookupSource := io.dequeueRequest.payload.vPifoId
    val frontPort = Mux(
      frontRewrite.io.lookupEnabled,
      frontRewrite.io.lookupTarget,
      io.dequeueRequest.payload.vPifoId
    )

    frontRewrite.io.drained << pifos.io.portDrained

    frontRewrite.io.emptySource.valid :=
      io.dequeueRequest.valid && frontRewrite.io.lookupCanEnable && pifos.io.popPortEmpty
    frontRewrite.io.emptySource.payload := io.dequeueRequest.payload.vPifoId

    val enablingRewrite =
      frontRewrite.io.drainEnablesRewrite || frontRewrite.io.emptyEnablesRewrite
    val pending = if (config.pipelinedCam)
      Reg(UInt(log2Up(config.lookupBufferDepth + 1) bits)) init(0) else null
    val room = if (config.pipelinedCam) pending < config.lookupBufferDepth else True
    pifos.io.popRequest.valid := io.dequeueRequest.valid && !enablingRewrite && room
    pifos.io.popRequest.port := frontPort
    io.dequeueRequest.ready := !enablingRewrite && room

    // An underflow is an invalid pop and produces no mesh message. A configured
    // transition enables before the next request, so it does not underflow.
    val existingPop = if (config.pipelinedCam) {
      val captured = new StreamFifo(PifoPopResponse(config), config.lookupBufferDepth,
        withAsyncRead = true, withBypass = true, useVec = true)
      captured.io.push.valid := pifos.io.popResponse.valid && pifos.io.popResponse.exist
      captured.io.push.payload := pifos.io.popResponse.payload
      assert(!captured.io.push.valid || captured.io.push.ready, "PIFO response credit overflow")
      val underflow = pifos.io.popResponse.valid && !pifos.io.popResponse.exist
      pending := pending + io.dequeueRequest.fire.asUInt - underflow.asUInt - io.dequeueResponse.fire.asUInt
      captured.io.pop
    } else pifos.io.popResponse.toStream.throwWhen(!pifos.io.popResponse.exist)
    val popResps = StreamFork(existingPop, 3)

    enque.brain.io.poped << popResps(0).toFlow

    dequeMapper.io.readReq << popResps(1).map(response => response.port @@ response.data)

    val popFifo = popResps(2).queueLowLatency(if (config.pipelinedCam) config.lookupBufferDepth else 2)
    StreamJoin(
      Seq(
        dequeMapper.io.readRes,
        popFifo
      )
    ).translateInto(io.dequeueResponse) { case (to, from) =>
      to.fromFlowId(dequeMapper.io.readRes.payload)
    }
  }

  val controller = new ControllerFactory(config)
  controller.dispatchStream(
    ControlCommand.UpdateMapperPre,
    enque.enqueMapper.io.writeReq
  ) { (to, from) =>
    to.inputId := from.vPifoId
    to.outputId := from.data.resized
  }

  controller.dispatchStream(
    ControlCommand.UpdateMapperPost,
    deque.dequeMapper.io.writeReq
  ) { (to, from) =>
    to.inputId := from.vPifoId @@ from.flowId
    to.outputId := from.data.resized
  }

  controller.dispatchStream(
    ControlCommand.UpdateMapperNonExist,
    deque.frontRewrite.io.writeReq
  ) { (to, from) =>
    to.inputId := from.vPifoId
    to.outputId := from.data(config.vpifoIdWidth - 1 downto 0)
  }

  val mapperCommitReady =
    enque.enqueMapper.io.commitReady &&
      deque.dequeMapper.io.commitReady
  io.commitReady := mapperCommitReady
  if (config.usesCam) {
    io.camCapacityBlocked := enque.brain.io.camCapacityBlocked || deque.dequeMapper.io.capacityBlocked
  }

  val (control, brainControl, commitControl) = StreamFork3(io.control)
  controller.build(control)
  enque.brain.io.control << brainControl

  val commit = commitControl.takeWhen(commitControl.payload.command === ControlCommand.CommitMapper)
  commit.ready := mapperCommitReady
  val commitPulse = if (config.dynamicConfig) commit.fire else False

  enque.enqueMapper.io.commit := commitPulse
  deque.dequeMapper.io.commit := commitPulse
  deque.frontRewrite.io.commit := commitPulse
}
