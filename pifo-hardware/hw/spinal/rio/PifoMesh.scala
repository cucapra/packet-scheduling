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
  // stop-the-world evaluation protocol
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

case class PifoMesh(config: EngineConfig) extends Component {
  val io = new Bundle {
    val dataRequest = slave(Stream(PifoMessage(config)))
    val pop = master(Stream(PifoMessage(config)))

    val insert = Vec(slave(Stream(PifoMessage(config))), config.numEngines)
    val controlRequest = slave(Stream(ControlMessage(config)))
    val commitReady = out Bool ()
    val commitEpoch = out Bool ()
    val rootEmpty = out Bool ()
  }

  // all datapath
  val xbar = MessageCrossBar(config)
  val pifoEngines = Seq.fill(config.numEngines)(PifoEngine(config))
  val trafficStopped = RegInit(False)
  val activeRootValid = RegInit(False)
  val activeRootEngine = Reg(UInt(config.engineIdWidth bits)) init (0)
  val activeRootPifo = Reg(UInt(config.vpifoIdWidth bits)) init (0)
  val pendingRootValid = RegInit(False)
  val pendingRootEngine = Reg(UInt(config.engineIdWidth bits)) init (0)
  val pendingRootPifo = Reg(UInt(config.vpifoIdWidth bits)) init (0)
  val stoppedRootEngine = Reg(UInt(config.engineIdWidth bits)) init (1)
  val stoppedRootPifo = Reg(UInt(config.vpifoIdWidth bits)) init (0)
  val stopSettling = Reg(UInt(6 bits)) init (0)
  val stopSnapshotValid = RegInit(False)
  val stoppedTokenCount = Reg(UInt((config.bitPifo + 1) bits)) init (0)
  val rootRequestsInFlight = Reg(UInt((config.bitPifo + 2) bits)) init (0)
  when(io.dataRequest.fire =/= io.pop.fire) {
    when(io.dataRequest.fire) { rootRequestsInFlight := rootRequestsInFlight + 1 }
      .otherwise { rootRequestsInFlight := rootRequestsInFlight - 1 }
  }

  val inspectPort = UInt(config.vpifoIdWidth bits)
  inspectPort := stoppedRootPifo
  pifoEngines.foreach(_.io.inspectPifo := inspectPort)
  val enginePortCounts = Vec(pifoEngines.map(_.io.inspectCount))
  val stoppedRootCount = enginePortCounts((stoppedRootEngine - 1).resized)
  pifoEngines.foreach(_.io.probePifo := Mux(activeRootValid, activeRootPifo, io.dataRequest.vPifoId))
  val rootStatuses = Vec(pifoEngines.map(_.io.probeEmpty))
  val rootEngine = Mux(activeRootValid, activeRootEngine, io.dataRequest.engineId)
  io.rootEmpty := rootStatuses((rootEngine - 1).resized)
  when(stopSettling =/= 0) { stopSettling := stopSettling - 1 }
  when(trafficStopped && stopSettling === 0 && rootRequestsInFlight === 0 && !stopSnapshotValid) {
    stoppedTokenCount := stoppedRootCount
    stopSnapshotValid := True
  }
  val copyController = PifoCopyController(config)
  val clearRequest = Flow(UInt(config.engineIdWidth bits))
  for (i <- 0 until config.numEngines) {
    val engine = pifoEngines(i)
    engine.io.copyIndex := copyController.io.index
    copyController.io.entries(i) << engine.io.copyEntry
    engine.io.copyInsert << copyController.io.insert(i)
    engine.io.copyClear := copyController.io.clear(i) || (clearRequest.valid && clearRequest.payload === i + 1)
    copyController.io.empty(i) := engine.io.copyEmpty
  }

  (pifoEngines zip xbar.io.outputs.tail).foreach { case (engine, out) =>
    engine.io.dequeueRequest << out
  }
  (pifoEngines zip xbar.io.inputs.tail).foreach { case (engine, in) =>
    engine.io.dequeueResponse >> in
  }

  // StopWorld freezes both insertion and root-pop admission. UpdateRoot is
  // published with the mapper commit, after the hardware prefill has finished.
  xbar.io.inputs(0).valid := io.dataRequest.valid && !trafficStopped
  io.dataRequest.ready := xbar.io.inputs(0).ready && !trafficStopped
  xbar.io.inputs(0).payload := io.dataRequest.payload
  when(activeRootValid) {
    xbar.io.inputs(0).payload.engineId := activeRootEngine
    xbar.io.inputs(0).payload.vPifoId := activeRootPifo
  }
  xbar.io.outputs(0) >> io.pop

  // insert path
  (io.insert zip pifoEngines).foreach { case (in, engine) =>
    engine.io.enqueRequest.valid := in.valid && !trafficStopped
    in.ready := engine.io.enqueRequest.ready && !trafficStopped
    engine.io.enqueRequest.payload := in.payload
  }

  // All control-plane commands are ordered through one hardware queue. The pre
  // and post mapper updates target backup banks; a commit is broadcast
  // synchronously so every engine changes those packet-visible mappings on the
  // same cycle. Underflow-rewrite entries are single-bank and never wait for
  // mapper bank synchronization.
  val controlQueue = io.controlRequest.queue(config.commitQueueLength)
  val mapperCommitReady = pifoEngines.map(_.io.commitReady).reduce(_ && _) &&
    !copyController.io.busy && (!trafficStopped || stopSnapshotValid)
  io.commitReady := mapperCommitReady
  val commitEpoch = RegInit(False)
  io.commitEpoch := commitEpoch

  val (routedHead, commitHead) = StreamFork2(controlQueue)

  val withoutCommit = routedHead.throwWhen(
    routedHead.payload.command === ControlCommand.CommitMapper
  )
  val isMapperUpdate =
    withoutCommit.payload.command === ControlCommand.UpdateMapperPre ||
      withoutCommit.payload.command === ControlCommand.UpdateMapperPost
  val waitingEmpty = withoutCommit.valid && withoutCommit.command === ControlCommand.WaitPifoEmpty
  when(waitingEmpty) { inspectPort := withoutCommit.vPifoId }
  val guardEmpty = enginePortCounts((withoutCommit.engineId - 1).resized) === 0
  val routedControl = withoutCommit.haltWhen(
    (isMapperUpdate && !mapperCommitReady) || copyController.io.busy ||
      (trafficStopped && !stopSnapshotValid) || (waitingEmpty && !guardEmpty)
  )

  val isMeshCommand =
    routedControl.payload.command === ControlCommand.StopWorld ||
      routedControl.payload.command === ControlCommand.UpdateRoot ||
      routedControl.payload.command === ControlCommand.CopyPifoEngine ||
      routedControl.payload.command === ControlCommand.WaitPifoEmpty ||
      routedControl.payload.command === ControlCommand.ClearPifoEngine
  clearRequest.valid := routedControl.fire && routedControl.command === ControlCommand.ClearPifoEngine
  clearRequest.payload := routedControl.engineId
  when(clearRequest.valid) {
    assert(trafficStopped || rootEngine =/= clearRequest.payload, "cannot reclaim the active root while running")
  }
  when(routedControl.fire && routedControl.payload.command === ControlCommand.StopWorld) {
    trafficStopped := True
    stoppedRootEngine := routedControl.payload.engineId
    stoppedRootPifo := routedControl.payload.vPifoId
    stopSettling := 32
    stopSnapshotValid := False
  }
  when(routedControl.fire && routedControl.payload.command === ControlCommand.UpdateRoot) {
    pendingRootValid := True
    pendingRootEngine := routedControl.payload.engineId
    pendingRootPifo := routedControl.payload.vPifoId
  }
  copyController.io.request.valid := routedControl.fire &&
    routedControl.payload.command === ControlCommand.CopyPifoEngine
  copyController.io.request.source := routedControl.payload.engineId
  copyController.io.request.target := routedControl.payload.data.resized
  when(copyController.io.request.valid) {
    assert(trafficStopped && stopSnapshotValid, "copy requires StopWorld quiescence")
  }

  val commitControl = commitHead
    .takeWhen(commitHead.payload.command === ControlCommand.CommitMapper)
    .haltWhen(!mapperCommitReady)

  when(commitControl.fire) {
    commitEpoch := !commitEpoch
    when(pendingRootValid) {
      activeRootValid := True
      activeRootEngine := pendingRootEngine
      activeRootPifo := pendingRootPifo
      pendingRootValid := False
    }
    when(trafficStopped) {
      trafficStopped := False
    }
  }

  val rawEngineControl = routedControl.throwWhen(isMeshCommand)
  val engineControl = Stream(ControlMessage(config))
  engineControl.valid := rawEngineControl.valid
  rawEngineControl.ready := engineControl.ready
  engineControl.payload := rawEngineControl.payload
  // A zero count is the stop-the-world runtime form: derive N from the frozen
  // old root's physical token occupancy. Non-zero data remains available for
  // direct hardware tests and low-level control programs.
  when(
    rawEngineControl.payload.command === ControlCommand.PrefillPifo &&
      rawEngineControl.payload.data === 0
  ) {
    engineControl.payload.data := Mux(trafficStopped, stoppedTokenCount, stoppedRootCount).resized
  }
  val translatedEngineId = (engineControl.payload.engineId - 1).resized
  val controlCommand = StreamDemux(engineControl, translatedEngineId, config.numEngines)
  // mapperCommitReady guarantees every destination can accept this item on its
  // first valid cycle. The default fork avoids a ready/valid combinational loop
  // through the per-engine arbiters while retaining same-cycle delivery.
  val commits = StreamFork(commitControl, config.numEngines)

  (controlCommand zip commits zip pifoEngines).foreach { case ((cmdStream, commitStream), engine) =>
    engine.io.control << StreamArbiterFactory.lowerFirst.onArgs(cmdStream, commitStream)
  }
}
