package rio

import spinal.core._
import spinal.lib._
import spinal.core

case class MessageCrossBar(config: EngineConfig) extends Component {
  val numPorts = config.numEngines + 1
  val io = new Bundle {
    val inputs = Vec(slave Stream (PifoMessage(config)), numPorts)
    val outputs = Vec(master Stream (PifoMessage(config)), numPorts)
  }

  val xbarFifoDepth = 8

  // TODO(zhiyuang): optimize the buffer to regs
  val fanouts = io.inputs.map { in =>
    val inFifo = in.queueLowLatency(xbarFifoDepth, latency = 1)
    StreamDemux(inFifo, inFifo.payload.engineId, numPorts)
  }

  for (i <- 0 until numPorts) {
    val arbiter = StreamArbiterFactory.lowerFirst.on(fanouts.map(_(i)))
    arbiter >-> io.outputs(i)
  }
}

object ControlCommand extends SpinalEnum {
  val UpdateMapperPre, UpdateMapperPost, UpdateMapperNonExist, CommitMapper,
  // brain operators
  UpdateBrainEngine, UpdateBrainState, UpdateBrainFlowState = newElement()
}

case class ControlMessage(config: EngineConfig) extends Bundle {
  val command = ControlCommand()
  val engineId = UInt(config.engineIdWidth bits)
  val vPifoId = UInt(config.vpifoIdWidth bits)
  val flowId = UInt(config.flowIdWidth bits)
  val data = UInt(config.flowStateWidth bits)
}

/** Only banked mapper writes are replayed. Store the bits their decoders use. */
case class MapperReplayInstruction(config: EngineConfig) extends Bundle {
  val post = Bool()
  val engineId = UInt(config.engineIdWidth bits)
  val vPifoId = UInt(config.vpifoIdWidth bits)
  val flowId = UInt(config.flowIdWidth bits)
  val data = UInt(config.flowIdWidth bits)
}

case class PifoMesh(config: EngineConfig) extends Component {
  val useReplay = config.dynamicConfig && config.mapperSync == "replay"
  val io = new Bundle {
    val dataRequest = slave(Stream(PifoMessage(config)))
    val pop = master(Stream(PifoMessage(config)))

    val insert = Vec(slave(Stream(PifoMessage(config))), config.numEngines)
    val controlRequest = slave(Stream(ControlMessage(config)))
    val commitReady = out Bool ()
    val pifo = if (config.pifoBackend == "external")
      Vec(master(PifoBoundary(config)), config.numEngines) else null
    val replayBusy = if (useReplay) out Bool() else null
    val replayLogAvailable = if (useReplay) out UInt(log2Up(config.replayLogDepth + 1) bits) else null
  }

  // all datapath
  val xbar = MessageCrossBar(config)
  val pifoEngines = Seq.fill(config.numEngines)(PifoEngine(config))
  if (config.pifoBackend == "external") {
    (io.pifo zip pifoEngines).foreach { case (boundary, engine) =>
      boundary <> engine.io.pifo
    }
  }

  (pifoEngines zip xbar.io.outputs.tail).foreach { case (engine, out) =>
    engine.io.dequeueRequest << out
  }
  (pifoEngines zip xbar.io.inputs.tail).foreach { case (engine, in) =>
    engine.io.dequeueResponse >> in
  }

  io.dataRequest >> xbar.io.inputs(0)
  xbar.io.outputs(0) >> io.pop

  // insert path
  (io.insert zip pifoEngines).foreach { case (in, engine) =>
    engine.io.enqueRequest << in
  }

  // All control-plane commands are ordered through one hardware queue. The pre
  // and post mapper updates target backup banks; a commit is broadcast
  // synchronously so every engine changes those packet-visible mappings on the
  // same cycle. Underflow-rewrite entries are single-bank and never wait for
  // mapper bank synchronization.
  val replaying = if (useReplay) RegInit(False) else null
  // Reserve log space at ingress, including mapper commands waiting in the
  // ordinary control FIFO. This prevents accepted updates from overflowing it.
  val reserved = if (useReplay) Reg(UInt(log2Up(config.replayLogDepth + 1) bits)) init(0) else null
  val ingress = if (useReplay) {
    val isMapper = io.controlRequest.command === ControlCommand.UpdateMapperPre ||
      io.controlRequest.command === ControlCommand.UpdateMapperPost
    io.controlRequest.haltWhen(replaying || (isMapper && reserved === config.replayLogDepth))
  } else io.controlRequest
  val controlQueue = ingress.queue(config.commitQueueLength)
  if (useReplay) {
    val log = StreamFifo(MapperReplayInstruction(config), config.replayLogDepth)
    io.replayBusy := replaying
    io.replayLogAvailable := U(config.replayLogDepth) - reserved
    val mapperCommitReady = pifoEngines.map(_.io.commitReady).reduce(_ && _)
    io.commitReady := !replaying && mapperCommitReady

    val staged = controlQueue.haltWhen(replaying)
    val (dispatchHead, recordHead) = StreamFork2(staged)
    val record = recordHead.takeWhen(recordHead.command === ControlCommand.UpdateMapperPre ||
      recordHead.command === ControlCommand.UpdateMapperPost)
    log.io.push << record.translateWith {
      val instruction = MapperReplayInstruction(config)
      instruction.post := record.command === ControlCommand.UpdateMapperPost
      instruction.engineId := record.engineId
      instruction.vPifoId := record.vPifoId
      instruction.flowId := record.flowId
      instruction.data := record.data.resized
      instruction
    }
    val (normalHead, commitHead) = StreamFork2(dispatchHead)
    val normal = normalHead.throwWhen(normalHead.command === ControlCommand.CommitMapper)
    val commit = commitHead.takeWhen(commitHead.command === ControlCommand.CommitMapper)
      .haltWhen(!mapperCommitReady)
    val replay = log.io.pop.haltWhen(!replaying).translateWith {
      val command = ControlMessage(config)
      command.command := Mux(log.io.pop.post, ControlCommand.UpdateMapperPost, ControlCommand.UpdateMapperPre)
      command.engineId := log.io.pop.engineId
      command.vPifoId := log.io.pop.vPifoId
      command.flowId := log.io.pop.flowId
      command.data := log.io.pop.data.resized
      command
    }
    val routed = StreamArbiterFactory.lowerFirst.onArgs(replay, normal)
    val commands = StreamDemux(routed, (routed.engineId - 1).resized, config.numEngines)
    val commits = StreamFork(commit, config.numEngines)
    (commands zip commits zip pifoEngines).foreach { case ((command, swap), engine) =>
      engine.io.control << StreamArbiterFactory.lowerFirst.onArgs(command, swap)
    }

    val reserve = ingress.fire && (ingress.command === ControlCommand.UpdateMapperPre ||
      ingress.command === ControlCommand.UpdateMapperPost)
    val release = log.io.pop.fire
    when(reserve =/= release) {
      when(reserve) { reserved := reserved + 1 } otherwise { reserved := reserved - 1 }
    }
    when(commit.fire && log.io.occupancy =/= 0) { replaying := True }
    when(release && log.io.occupancy === 1) { replaying := False }
  } else if (config.dynamicConfig) {
  val mapperCommitReady = pifoEngines.map(_.io.commitReady).reduce(_ && _)
  io.commitReady := mapperCommitReady

  val (routedHead, commitHead) = StreamFork2(controlQueue)

  val withoutCommit = routedHead.throwWhen(
    routedHead.payload.command === ControlCommand.CommitMapper
  )
  val isMapperUpdate =
    withoutCommit.payload.command === ControlCommand.UpdateMapperPre ||
      withoutCommit.payload.command === ControlCommand.UpdateMapperPost
  val routedControl = withoutCommit.haltWhen(isMapperUpdate && !mapperCommitReady)

  val commitControl = commitHead
    .takeWhen(commitHead.payload.command === ControlCommand.CommitMapper)
    .haltWhen(!mapperCommitReady)

  val translatedEngineId = (routedControl.payload.engineId - 1).resized
  val controlCommand = StreamDemux(routedControl, translatedEngineId, config.numEngines)
  // mapperCommitReady guarantees every destination can accept this item on its
  // first valid cycle. The default fork avoids a ready/valid combinational loop
  // through the per-engine arbiters while retaining same-cycle delivery.
  val commits = StreamFork(commitControl, config.numEngines)

  (controlCommand zip commits zip pifoEngines).foreach { case ((cmdStream, commitStream), engine) =>
    engine.io.control << StreamArbiterFactory.lowerFirst.onArgs(cmdStream, commitStream)
  }
  } else {
    // Retain the command queue, decoder, and per-engine configuration routing.
    // Commit occupies its normal ingress slot, then is consumed without a
    // broadcast, bank swap, synchronization walk, or packet-path hold.
    io.commitReady := True
    val routedControl = controlQueue.throwWhen(
      controlQueue.payload.command === ControlCommand.CommitMapper
    )
    val translatedEngineId = (routedControl.payload.engineId - 1).resized
    val controlCommand = StreamDemux(routedControl, translatedEngineId, config.numEngines)
    (controlCommand zip pifoEngines).foreach { case (command, engine) =>
      engine.io.control << command
    }
  }
}
