package rio

import spinal.core._
import spinal.lib._

// flow = vPIFOs + FlowId

case class EngineConfig (
    numEngines : Int,
    numVPIFOs : Int,
    maxPacketPriority: Int,
    fifoDepth: Int,
    prefetchBufferDepth: Int,

    brainStateWidth : Int = 32,
    flowStateWidth : Int = 32,

    configDataWidth : Int = 32
) {
    def vpifoIdWidth = log2Up(numVPIFOs)
    def numFlows = numVPIFOs * numEngines
    def engineIdWidth = log2Up(numEngines)
    def flowIdWidth = vpifoIdWidth + engineIdWidth

    def numBrainState = 1 << brainStateWidth
    def numFlowState = 1 << flowStateWidth

    assert(flowStateWidth <= configDataWidth, "flowStateWidth should be less than configDataWidth")
    assert(brainStateWidth % configDataWidth == 0, "brainStateWidth should be multiple of configDataWidth")
}

object EngineConfig {
  implicit def toFlowPifoConfig(pifoConfig: EngineConfig): PifoConfig =
    PifoConfig(
      numPifo = pifoConfig.numVPIFOs * pifoConfig.fifoDepth,
      bitPort = pifoConfig.vpifoIdWidth,
      bitPrio = log2Up(pifoConfig.maxPacketPriority),
      bitData = pifoConfig.flowIdWidth
    )
}

case class MapperUpdater(inputWidth : Int, outputWidth : Int) extends Bundle {
  val inputId = UInt(inputWidth bits)
  val outputId = UInt(outputWidth bits)
}

// TODO(zhiyuang): check for the writeFirst policy
case class Mapper(inputWidth : Int, outputWidth : Int) extends Component {
  val numInputs = 1 << inputWidth
  def updater = MapperUpdater(inputWidth, outputWidth)

  val io = new Bundle {
    val readReq = slave Flow(UInt(inputWidth bits))
    val readRes = master Flow(UInt(outputWidth bits))

    val writeReq = slave Flow(updater)
  }

  val ram = Mem(UInt(outputWidth bits), numInputs) init (Seq.fill(numInputs)(0))

  // read logic
  ram.flowReadSync(io.readReq) >> io.readRes

  // write logic
  ram.writePort().translateFrom(io.writeReq) { (to, from) =>
    to.address := from.inputId
    to.data := from.outputId
  }
}

case class BrainInput(config : EngineConfig) extends Bundle {
  val vpifoId = UInt(config.vpifoIdWidth bits)
  val flowId = UInt(config.flowIdWidth bits)
}

case class PIFOBrain(config : EngineConfig) extends Component {
  val io = new Bundle {
    val request = slave Stream(BrainInput(config))
    val response = master Stream(PifoEntry(config))

    val control = slave Stream(ControlMessage(config))
    val poped = slave Flow(PifoPopResponse(config))
  }

  val inHeads = StreamFork(io.request, 5)
  val controller = new ControllerFactory(config)

  val engineMapper = Mapper(config.vpifoIdWidth, log2Up(BrainType.elements.size))
  inHeads(0).map(_.vpifoId).toFlow >> engineMapper.io.readReq
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
  inHeads(1).map(_.vpifoId).toFlow >> lastVirtualMapper.io.readReq

  val engineCAM = Mapper(config.flowIdWidth + config.vpifoIdWidth, config.flowStateWidth)
  inHeads(2).map { data =>
    data.vpifoId @@ data.flowId
  }.toFlow >> engineCAM.io.readReq
  // TODO(zhiyuang): check this priority and flows when updating
  val flowStateControl, flowStateUpdate = Flow(engineCAM.updater)
  engineCAM.io.writeReq << StreamArbiterFactory
    .lowerFirst
    .onArgs(flowStateControl.toStream, flowStateUpdate.toStream.queueLowLatency(2))
    .toFlow
  controller.dispatch(
    ControlCommand.UpdateBrainFlowState,
    flowStateControl
  ) { (to, from) =>
    to.inputId := from.vPifoId @@ from.flowId
    to.outputId := from.data.resized
  }

  val brainStateMem = Mapper(config.vpifoIdWidth, config.brainStateWidth)
  inHeads(3).map { _.vpifoId }.toFlow >> brainStateMem.io.readReq
  val brainStateControl, brainStateUpdate = Flow(brainStateMem.updater)
  brainStateMem.io.writeReq << StreamArbiterFactory
    .lowerFirst
    .onArgs(brainStateControl.toStream, brainStateUpdate.toStream.queueLowLatency(2))
    .toFlow
  controller.dispatch(
    ControlCommand.UpdateBrainState,
    brainStateControl
  ) { (to, from) =>
    to.inputId := from.vPifoId
    to.outputId := from.data.resized
  }
  
  controller.build(io.control)


  val engineFifo = engineMapper.io.readRes.toStream.queueLowLatency(2)
  val brainStateFifo = brainStateMem.io.readRes.toStream.queueLowLatency(2)
  val flowStateFifo = engineCAM.io.readRes.toStream.queueLowLatency(2)
  val lastVirtualFifo = lastVirtualMapper.io.readRes.toStream.queueLowLatency(2)
  val inputFifo = inHeads(4).queueLowLatency(2)

  val engineStream = StreamJoin(Seq(
    engineFifo,
    brainStateFifo,
    flowStateFifo,
    lastVirtualFifo,
    inputFifo
  )).map { data =>
    val anno = new Bundle {
      val pifoId      = cloneOf(inHeads(4).payload.vpifoId)
      val flowId      = cloneOf(inHeads(4).payload.flowId)
      val engineId    = cloneOf(engineMapper.io.readRes.payload)
      val flowState   = cloneOf(engineCAM.io.readRes.payload)
      val brainState  = cloneOf(brainStateMem.io.readRes.payload)
      val virutalTime = cloneOf(lastVirtualMapper.io.readRes.payload)
    }

    anno.pifoId      := inputFifo.payload.vpifoId
    anno.flowId      := inputFifo.payload.flowId
    anno.engineId    := engineFifo.payload
    anno.flowState   := flowStateFifo.payload
    anno.brainState  := brainStateFifo.payload
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
        res.flowUpdate.flow.outputId :=  newTime.resized
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

  io.response << output
    .throwWhen(output.entry.priority === 0)
    .map(_.entry)

  flowStateUpdate << flowUpdates
    .throwWhen(!flowUpdates.payload.flowUpdate.update)
    .map(_.flowUpdate.flow).toFlow

  brainStateUpdate << brainUpdates
    .throwWhen(!brainUpdates.payload.brainUpdate.update)
    .map(_.brainUpdate.brain).toFlow
}

case class PifoMessage(config: EngineConfig) extends Bundle {
  val engineId = UInt(config.engineIdWidth bits)
  val vPifoId = UInt(config.vpifoIdWidth bits)

  def flowId : UInt = engineId @@ vPifoId
  def fromFlowId(id : UInt) = {
    engineId := id(config.flowIdWidth - 1 downto config.vpifoIdWidth)
    vPifoId := id(config.vpifoIdWidth - 1 downto 0)
  }
}

object PifoMessage {
  def fromData(config : EngineConfig, data : UInt, exist : Bool) : PifoMessage = {
    val msg = PifoMessage(config)
    // If not exist, set engineId to 0
    msg.engineId := Mux(exist, data(config.flowIdWidth - 1 downto config.vpifoIdWidth), U(0))
    msg.vPifoId := data(config.vpifoIdWidth - 1 downto 0)
    msg
  }
}

case class PifoEngine(config : EngineConfig) extends Component {
  val io = new Bundle {
    val enqueRequest = slave Stream(PifoMessage(config))
    val dequeueRequest = slave Stream(PifoMessage(config))

    val dequeueResponse = master Stream(PifoMessage(config))

    // control signals
    val control = slave Stream(ControlMessage(config))
  }

  // PIFO
  val pifos = new PifoRTL(config)

  // enque logic
  // enqueMapper maps flowIds to VPIFO ids
  val enque = new Area {
    val (mapperRead, flowIdStream) = StreamFork2(io.enqueRequest)

    val enqueMapper = Mapper(config.vpifoIdWidth, config.vpifoIdWidth)
    enqueMapper.io.readReq << mapperRead.map(_.vPifoId).toFlow

    val brainInput = Stream(BrainInput(config))
    StreamJoin(enqueMapper.io.readRes.toStream, flowIdStream.queueLowLatency(2))
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
    // dequeue also need a mapper, reinterpret vpifoId if needed
    val dequeMapper = Mapper(config.flowIdWidth, config.flowIdWidth)

    pifos.io.popRequest.translateFrom(io.dequeueRequest.toFlow) { case (to, from) =>
      to.port := from.vPifoId
    }

    val (popResp, brainUpdate) = StreamFork2(pifos.io.popResponse.toStream)
    enque.brain.io.poped << brainUpdate.toFlow

    // TODO(zhiyuang): handle not exist. do not drop it
    popResp
      .throwWhen(!popResp.exist)
      .map(_.data).toFlow >> dequeMapper.io.readReq
    
    io.dequeueResponse.translateFrom(dequeMapper.io.readRes.toStream) { _.fromFlowId(_) }
  }

  val controller = new ControllerFactory(config)
  controller.dispatch(
    ControlCommand.UpdateMapperPre,
    enque.enqueMapper.io.writeReq
  ) { (to, from) =>
    to.inputId := from.vPifoId
    to.outputId := from.data.resized
  }

  controller.dispatch(
    ControlCommand.UpdateMapperPost,
    deque.dequeMapper.io.writeReq
  ) { (to, from) =>
    to.inputId := from.flowId
    to.outputId := from.data.resized
  }

  val (control, brainControl) = StreamFork2(io.control)
  controller.build(control)
  enque.brain.io.control << brainControl
}