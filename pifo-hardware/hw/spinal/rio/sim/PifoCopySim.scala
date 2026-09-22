package rio.sim

import scala.collection.mutable.ArrayBuffer
import spinal.core.sim._
import rio._

/** A real copy never traverses the ordinary dequeue/sorter datapath. */
object PifoCopySim extends App {
  val config = EngineConfig(2, 8, 256, 8, 4)
  SimConfig.withIVerilog.addSimulatorFlag("-g2012").compile {
    val mesh = new EvaluationPifoMesh(config)
    mesh.maintenance.trafficStopped.simPublic()
    mesh.maintenance.copyController.io.busy.simPublic()
    mesh.maintenance.copyController.io.copied.simPublic()
    mesh.pifoEngines.foreach { engine =>
      engine.io.copyEmpty.simPublic()
      engine.io.copyInsert.simPublic()
      engine.pifos.io.popRequest.valid.simPublic()
    }
    mesh
  }.doSim { dut =>
    val controller = PifoMeshSimController(config, dut)
    controller.start(false, SimUtils.DefaultControlSocketPath, false)
    dut.io.dataRequest.engineId #= 1
    dut.io.dataRequest.vPifoId #= 1
    dut.clockDomain.waitSampling(2)
    assert(dut.io.rootEmpty.toBoolean, "an unused root must probe empty before its first pop")
    fork { dut.clockDomain.waitSampling(20000); simFailure("copy timeout") }
    for ((flow, rank) <- Seq(1 -> 7, 2 -> 1, 3 -> 3)) {
      controller.sendControl(ControlCommand.UpdateBrainEngine, 1, 2, 1)
      controller.sendControl(ControlCommand.UpdateBrainFlowState, 1, rank, 1, controller.mkFlowId(1, flow))
      controller.sendControl(ControlCommand.UpdateMapperPre, 1, 1, flow)
      // Destination mappings intentionally use the ORIGINAL packed token IDs.
      controller.sendControl(ControlCommand.UpdateMapperPost, 2, flow, 1, controller.mkFlowId(1, flow))
    }
    controller.sendControl(ControlCommand.CommitMapper, 1, 0)
    for (flow <- Seq(1, 2, 3, 2)) { controller.enque(flow); dut.clockDomain.waitSampling(12) }
    assert(!dut.io.rootEmpty.toBoolean, "root occupancy must not depend on a prior pop request")
    val copied = ArrayBuffer.empty[(Int, Int, Int)]
    val popped = ArrayBuffer.empty[Int]
    fork {
      while (true) {
        dut.clockDomain.waitSampling()
        val target = dut.pifoEngines(1).io.copyInsert
        if (target.valid.toBoolean && target.ready.toBoolean)
          copied += ((target.port.toInt, target.priority.toInt, target.data.toInt))
        if (dut.maintenance.copyController.io.busy.toBoolean)
          assert(dut.pifoEngines.forall(!_.pifos.io.popRequest.valid.toBoolean))
        if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) popped += dut.io.pop.vPifoId.toInt
      }
    }
    controller.sendControl(ControlCommand.StopWorld, 1, 0, 1)
    controller.sendControl(ControlCommand.CopyPifoEngine, 1, 2)
    controller.sendControl(ControlCommand.CommitMapper, 1, 0)
    assert(dut.pifoEngines(0).io.copyEmpty.toBoolean)
    assert(!dut.pifoEngines(1).io.copyEmpty.toBoolean)
    assert(copied.toVector == Vector(2 -> 1, 2 -> 1, 3 -> 3, 1 -> 7).map {
      case (flow, rank) => (1, rank, controller.mkFlowId(1, flow))
    }, copied.mkString(","))
    for (_ <- 0 until 4) { controller.requestDequeue(2, 1); dut.clockDomain.waitSampling(15) }
    assert(popped.toVector == Vector(2, 2, 3, 1), popped.mkString(","))
    assert(dut.pifoEngines(1).io.copyEmpty.toBoolean)
    // Reclaim detached synthetic entries without touching the active root.
    controller.sendControl(ControlCommand.PrefillPifo, 1, 3, 1, controller.mkFlowId(1, 7))
    dut.clockDomain.waitSampling(12)
    assert(!dut.pifoEngines(0).io.copyEmpty.toBoolean)
    controller.sendControl(ControlCommand.ClearPifoEngine, 1, 0)
    controller.sendControl(ControlCommand.CommitMapper, 1, 0)
    assert(dut.pifoEngines(0).io.copyEmpty.toBoolean)
    println("[PifoCopySim] rank/order preserved; source cleared; zero ordinary copy pops")
  }
}
