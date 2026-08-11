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

case class PifoMesh(config: EngineConfig) extends Component {
  val io = new Bundle {
    val dataRequest = slave(Stream(PifoMessage(config)))
    val pop = master(Stream(PifoMessage(config)))

    val insert = Vec(slave(Stream(PifoMessage(config))), config.numEngines)
    val controlRequest = slave(Stream(ControlMessage(config)))
    val commitReady = out Bool ()
  }

  // all datapath
  val xbar = MessageCrossBar(config)
  val pifoEngines = Seq.fill(config.numEngines)(PifoEngine(config))

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

  // All control-plane commands are ordered through one hardware queue. Mapper
  // updates target backup banks; a commit is broadcast synchronously so every
  // engine changes its packet-visible mappings on the same cycle.
  val controlQueue = io.controlRequest.queue(config.commitQueueLength)
  val mapperCommitReady = pifoEngines.map(_.io.commitReady).reduce(_ && _)
  io.commitReady := mapperCommitReady

  val (routedHead, commitHead) = StreamFork2(controlQueue)

  val withoutCommit = routedHead.throwWhen(
    routedHead.payload.command === ControlCommand.CommitMapper
  )
  val isMapperUpdate =
    withoutCommit.payload.command === ControlCommand.UpdateMapperPre ||
      withoutCommit.payload.command === ControlCommand.UpdateMapperPost ||
      withoutCommit.payload.command === ControlCommand.UpdateMapperNonExist
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
}
