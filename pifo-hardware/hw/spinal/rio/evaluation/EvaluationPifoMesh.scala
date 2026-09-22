package rio

import spinal.core._
import spinal.lib._

/** Explicit evaluation top level. PifoMesh's normal build never elaborates this area. */
class EvaluationPifoMesh(config: EngineConfig) extends PifoMesh(config, evaluation = true)

/** Hardware stop, counted token prefill, root publication and frozen PE relocation.
  * Commands still use the production shared replay FIFO and GuardDrain.
  * Maintenance commands are unbanked and therefore execute only on the first pass.
  */
class EvaluationMeshControl(mesh: PifoMesh, config: EngineConfig) extends Area {
  val control = Stream(ControlMessage(config))
  val forwarded = Stream(ControlMessage(config))
  val commit = Bool()
  val engines = mesh.pifoEngines
  val trafficStopped = RegInit(False)
  val activeRootValid = RegInit(False)
  val activeRootEngine = Reg(UInt(config.engineIdWidth bits)) init(0)
  val activeRootPifo = Reg(UInt(config.vpifoIdWidth bits)) init(0)
  val pendingRootValid = RegInit(False)
  val pendingRootEngine = Reg(UInt(config.engineIdWidth bits)) init(0)
  val pendingRootPifo = Reg(UInt(config.vpifoIdWidth bits)) init(0)
  val stoppedRootEngine = Reg(UInt(config.engineIdWidth bits)) init(1)
  val stoppedRootPifo = Reg(UInt(config.vpifoIdWidth bits)) init(0)
  val stopSettling = Reg(UInt(6 bits)) init(0)
  val stopSnapshotValid = RegInit(False)
  val stoppedTokenCount = Reg(UInt((config.bitPifo + 1) bits)) init(0)
  val rootRequestsInFlight = Reg(UInt((config.bitPifo + 2) bits)) init(0)
  when(mesh.io.dataRequest.fire =/= mesh.io.pop.fire) {
    when(mesh.io.dataRequest.fire) { rootRequestsInFlight := rootRequestsInFlight + 1 }
      .otherwise { rootRequestsInFlight := rootRequestsInFlight - 1 }
  }
  val inspectPort = UInt(config.vpifoIdWidth bits)
  inspectPort := stoppedRootPifo
  engines.foreach(_.io.inspectPifo := inspectPort)
  val enginePortCounts = Vec(engines.map(_.io.inspectCount))
  val stoppedRootCount = enginePortCounts((stoppedRootEngine - 1).resized)
  val rootEngine = Mux(activeRootValid, activeRootEngine, mesh.io.dataRequest.engineId)
  // A detached wrapper may still have root visits queued in the crossbar.
  // Wrapper PEs are dedicated; every visit to one is a root visit.
  val pendingRootVisits = Vec.fill(config.numEngines)(Reg(UInt((config.bitPifo + 2) bits)) init(0))
  for (index <- engines.indices) {
    val arriving = mesh.io.dataRequest.fire && rootEngine === index + 1
    val visiting = engines(index).io.dequeueRequest.fire && pendingRootVisits(index) =/= 0
    when(arriving =/= visiting) {
      when(arriving) { pendingRootVisits(index) := pendingRootVisits(index) + 1 }
        .otherwise { pendingRootVisits(index) := pendingRootVisits(index) - 1 }
    }
  }
  engines.foreach(_.io.probePifo := Mux(activeRootValid, activeRootPifo, mesh.io.dataRequest.vPifoId))
  val rootStatuses = Vec(engines.map(_.io.probeEmpty))
  mesh.io.rootEmpty := rootStatuses((rootEngine - 1).resized)
  when(stopSettling =/= 0) { stopSettling := stopSettling - 1 }
  when(trafficStopped && stopSettling === 0 && rootRequestsInFlight === 0 && !stopSnapshotValid) {
    stoppedTokenCount := stoppedRootCount
    stopSnapshotValid := True
  }

  val copyController = PifoCopyController(config)
  val clearRequest = Flow(UInt(config.engineIdWidth bits))
  for (i <- 0 until config.numEngines) {
    val engine = engines(i)
    engine.io.copyIndex := copyController.io.index
    copyController.io.entries(i) << engine.io.copyEntry
    engine.io.copyInsert << copyController.io.insert(i)
    engine.io.copyClear := copyController.io.clear(i) || (clearRequest.valid && clearRequest.payload === i + 1)
    copyController.io.empty(i) := engine.io.copyEmpty
  }
  val commitReady = !copyController.io.busy && (!trafficStopped || stopSnapshotValid)

  mesh.xbar.io.inputs(0).valid := mesh.io.dataRequest.valid && !trafficStopped
  mesh.io.dataRequest.ready := mesh.xbar.io.inputs(0).ready && !trafficStopped
  mesh.xbar.io.inputs(0).payload := mesh.io.dataRequest.payload
  when(activeRootValid) {
    mesh.xbar.io.inputs(0).engineId := activeRootEngine
    mesh.xbar.io.inputs(0).vPifoId := activeRootPifo
  }
  (mesh.io.insert zip engines).foreach { case (in, engine) =>
    engine.io.enqueRequest.valid := in.valid && !trafficStopped
    in.ready := engine.io.enqueRequest.ready && !trafficStopped
    engine.io.enqueRequest.payload := in.payload
  }

  // WaitPifoEmpty is kept for old direct programs. New compilers use GuardDrain.
  val waitingEmpty = control.valid && control.command === ControlCommand.WaitPifoEmpty
  when(waitingEmpty) { inspectPort := control.vPifoId }
  val guardEmpty = enginePortCounts((control.engineId - 1).resized) === 0
  val engineIdle = Vec(engines.map(_.io.maintenanceIdle))
  val clearTarget = (control.engineId - 1).resized
  val clearWaiting = control.command === ControlCommand.ClearPifoEngine &&
    (pendingRootVisits(clearTarget) =/= 0 || !engineIdle(clearTarget))
  val routedControl = control.haltWhen(copyController.io.busy ||
    (trafficStopped && !stopSnapshotValid) || (waitingEmpty && !guardEmpty) || clearWaiting)
  val isMeshCommand = routedControl.command === ControlCommand.StopWorld ||
    routedControl.command === ControlCommand.UpdateRoot ||
    routedControl.command === ControlCommand.CopyPifoEngine ||
    routedControl.command === ControlCommand.WaitPifoEmpty ||
    routedControl.command === ControlCommand.ClearPifoEngine
  clearRequest.valid := routedControl.fire && routedControl.command === ControlCommand.ClearPifoEngine
  clearRequest.payload := routedControl.engineId
  when(clearRequest.valid) {
    assert(trafficStopped || rootEngine =/= clearRequest.payload, "cannot reclaim the active root while running")
  }
  when(routedControl.fire && routedControl.command === ControlCommand.StopWorld) {
    trafficStopped := True
    stoppedRootEngine := routedControl.engineId
    stoppedRootPifo := routedControl.vPifoId
    stopSettling := 32
    stopSnapshotValid := False
  }
  when(routedControl.fire && routedControl.command === ControlCommand.UpdateRoot) {
    pendingRootValid := True
    pendingRootEngine := routedControl.engineId
    pendingRootPifo := routedControl.vPifoId
  }
  copyController.io.request.valid := routedControl.fire && routedControl.command === ControlCommand.CopyPifoEngine
  copyController.io.request.source := routedControl.engineId
  copyController.io.request.target := routedControl.data.resized
  when(copyController.io.request.valid) {
    assert(trafficStopped && stopSnapshotValid, "copy requires StopWorld quiescence")
  }
  when(commit) {
    when(pendingRootValid) {
      activeRootValid := True
      activeRootEngine := pendingRootEngine
      activeRootPifo := pendingRootPifo
      pendingRootValid := False
    }
    trafficStopped := False
  }
  val raw = routedControl.throwWhen(isMeshCommand)
  forwarded << raw
  // data=0 means use the old root's hardware occupancy snapshot, not a host count.
  when(raw.command === ControlCommand.PrefillPifo && raw.data === 0) {
    forwarded.data := Mux(trafficStopped, stoppedTokenCount, stoppedRootCount).resized
  }
}
