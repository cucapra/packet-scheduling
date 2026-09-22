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
  UpdateBrainEngine, UpdateBrainState, UpdateBrainFlowState,
  GuardDrain,
  // Evaluation-only opcodes. Production has no decoder/datapath for these.
  StopWorld, PrefillPifo, UpdateRoot, CopyPifoEngine,
  UpdateRankGroup, UpdateRankQuantum, WaitPifoEmpty, ClearPifoEngine = newElement()
}

case class ControlMessage(config: EngineConfig) extends Bundle {
  val command = ControlCommand()
  val engineId = UInt(config.engineIdWidth bits)
  val vPifoId = UInt(config.vpifoIdWidth bits)
  val flowId = UInt(config.flowIdWidth bits)
  val data = UInt(config.flowStateWidth bits)
}

class PifoMesh private[rio](config: EngineConfig, val evaluation: Boolean) extends Component {
  def this(config: EngineConfig) = this(config, false)
  val io = new Bundle {
    val dataRequest = slave(Stream(PifoMessage(config)))
    val pop = master(Stream(PifoMessage(config)))

    val insert = Vec(slave(Stream(PifoMessage(config))), config.numEngines)
    val controlRequest = slave(Stream(ControlMessage(config)))
    val commitReady = out Bool ()
    val commitEpoch = out UInt (32 bits)
    val replayBusy = out Bool()
    // Free non-commit slots in the shared command FIFO; excludes commit reserve.
    val replayLogAvailable = out UInt(log2Up(config.commitQueueLength + 1) bits)
    val rootEmpty = evaluation generate (out Bool())
  }

  // all datapath
  val xbar = MessageCrossBar(config)
  val pifoEngines = Seq.fill(config.numEngines)(
    if (evaluation) new EvaluationPifoEngine(config) else PifoEngine(config)
  )
  val maintenance = evaluation generate new EvaluationMeshControl(this, config)

  (pifoEngines zip xbar.io.outputs.tail).foreach { case (engine, out) =>
    engine.io.dequeueRequest << out
  }
  (pifoEngines zip xbar.io.inputs.tail).foreach { case (engine, in) =>
    engine.io.dequeueResponse >> in
  }

  if (!evaluation) io.dataRequest >> xbar.io.inputs(0)
  xbar.io.outputs(0) >> io.pop

  // insert path
  if (!evaluation) (io.insert zip pifoEngines).foreach { case (in, engine) =>
    engine.io.enqueRequest << in
  }

  // All control-plane commands are ordered through one hardware queue. The pre
  // and post mapper updates target backup banks; a commit is broadcast
  // synchronously so every engine changes those packet-visible mappings on the
  // same cycle. Underflow-rewrite entries remain single-bank and execute only
  // on the first pass through the command FIFO.
  val replayControl = ReplayControlFifo(config)
  replayControl.io.push << io.controlRequest
  val controlQueue = replayControl.io.pop
  io.replayBusy := replayControl.io.replaying
  io.replayLogAvailable := replayControl.io.available
  val mapperCommitReady = pifoEngines.map(_.io.commitReady).reduce(_ && _) &&
    (if (evaluation) maintenance.commitReady else True)
  io.commitReady := !replayControl.io.replaying && mapperCommitReady

  val guard = DrainGuard(config)
  (pifoEngines zipWithIndex).foreach { case (engine, index) =>
    guard.io.nearlyDrained(index) << engine.io.nearlyDrained
    (guard.io.pushed(index) zip engine.io.pushed).foreach { case (in, out) => in << out }
  }
  guard.io.engineId := controlQueue.payload.engineId
  guard.io.vPifoId := controlQueue.payload.vPifoId
  val isGuard = controlQueue.payload.command === ControlCommand.GuardDrain
  // Gate before the fork: no later command (including a commit or immediate
  // brain write) may pass a blocked guard. Guards are consumed here, not at a PE.
  val guardedHead = controlQueue.haltWhen(isGuard && !guard.io.satisfied).throwWhen(isGuard)
  val controlledHead = if (evaluation) {
    maintenance.control << guardedHead
    maintenance.forwarded
  } else guardedHead
  val (routedHead, commitHead) = StreamFork2(controlledHead)

  val withoutCommit = routedHead.throwWhen(
    routedHead.payload.command === ControlCommand.CommitMapper
  )

  val commitControl = commitHead
    .takeWhen(commitHead.payload.command === ControlCommand.CommitMapper)
    .haltWhen(!mapperCommitReady)
  if (evaluation) maintenance.commit := commitControl.fire
  val routedControl = if (evaluation) maintenance.routedControl else withoutCommit

  val translatedEngineId = (withoutCommit.payload.engineId - 1).resized
  val controlCommand = StreamDemux(withoutCommit, translatedEngineId, config.numEngines)
  // mapperCommitReady guarantees every destination can accept this item on its
  // first valid cycle. The default fork avoids a ready/valid combinational loop
  // through the per-engine arbiters while retaining same-cycle delivery.
  val commits = StreamFork(commitControl, config.numEngines)
  val commitEpoch = Reg(UInt(32 bits)) init (0)
  when(commitControl.fire) { commitEpoch := commitEpoch + 1 }
  io.commitEpoch := commitEpoch

  (controlCommand zip commits zip pifoEngines).foreach { case ((cmdStream, commitStream), engine) =>
    engine.io.control << StreamArbiterFactory.lowerFirst.onArgs(cmdStream, commitStream)
  }
}

object PifoMesh {
  def apply(config: EngineConfig): PifoMesh = new PifoMesh(config)
}
