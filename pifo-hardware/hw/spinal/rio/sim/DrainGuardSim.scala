package rio.sim

import scala.collection.mutable.ArrayBuffer
import spinal.core.sim._
import rio._

/** Focused guard/ordinary-cleanup integration test, not an experiment sweep.
  * sbt "runMain rio.sim.DrainGuardSim"
  */
object DrainGuardSim extends App {
  val config = EngineConfig(2, 4, 256, 2, 2)
  SimConfig.withIVerilog.addSimulatorFlag("-g2012").compile {
    val mesh = PifoMesh(config)
    mesh.pifoEngines.foreach { engine =>
      engine.enque.brain.engineMapper.io.writeReq.valid.simPublic()
      engine.enque.brain.engineMapper.io.writeReq.payload.outputId.simPublic()
      engine.enque.brain.io.request.valid.simPublic()
      engine.enque.brain.io.request.payload.vpifoId.simPublic()
    }
    mesh
  }.doSim { dut =>
    SimTimeout(100000)
    dut.io.dataRequest.valid #= false
    dut.io.pop.ready #= true
    dut.io.controlRequest.valid #= false
    dut.io.insert.foreach(_.valid #= false)
    val controller = PifoMeshSimController(config, dut)
    controller.start(false, SimUtils.DefaultControlSocketPath, monitorPops = false)

    val pops = ArrayBuffer.empty[Int]
    var disabledBrains = 0
    fork {
      while (true) {
        dut.clockDomain.waitSampling()
        if (dut.io.pop.valid.toBoolean) pops += dut.io.pop.payload.vPifoId.toInt
        for (engine <- dut.pifoEngines) {
          val write = engine.enque.brain.engineMapper.io.writeReq
          if (write.valid.toBoolean && write.payload.outputId.toInt == 0) disabledBrains += 1
        }
      }
    }

    def send(command: ControlCommand.E, engine: Int, port: Int = 0, data: Int = 0, flow: Int = 0): Unit =
      controller.sendControl(command, engine, data, vPifoId = port, flowId = flow)

    for (engine <- 1 to 2; port <- 1 to 3) {
      send(ControlCommand.UpdateBrainEngine, engine, port, 3)
      send(ControlCommand.UpdateMapperPre, engine, port, port)
      send(ControlCommand.UpdateMapperPost, engine, port, port, controller.mkFlowId(engine, port))
    }
    send(ControlCommand.CommitMapper, 1)

    controller.enqueueToEngine(1, 1) // right FIFO, wrong PE
    controller.enqueueToEngine(2, 2) // right PE, wrong FIFO
    controller.enqueueToEngine(2, 1) // guarded FIFO
    dut.clockDomain.waitSampling(16)
    val epoch = dut.io.commitEpoch.toBigInt
    var cleaned = false
    val accepted = ArrayBuffer.empty[Long]
    val cleanup = fork {
      val instructions = Vector(
        RequestControlInstruction(ControlCommand.GuardDrain, 2, 1, 0, 0),
        RequestControlInstruction(ControlCommand.UpdateBrainEngine, 2, 1, 0, 0),
        RequestControlInstruction(ControlCommand.UpdateMapperPre, 2, 1, 0, 0),
        RequestControlInstruction(ControlCommand.UpdateMapperPost, 2, 1, controller.mkFlowId(2, 1), 0),
        RequestControlInstruction(ControlCommand.CommitMapper, 1, 0, 0, 0)
      )
      instructions.foreach { command =>
        controller.sendControl(command.command, command.engineId, command.data,
          command.vPifoId, command.flowId, onAccepted = () => accepted += simTime())
      }
      cleaned = true
    }
    dut.clockDomain.waitSampling(12)
    assert(disabledBrains == 0 && dut.io.commitEpoch.toBigInt == epoch && !cleaned,
      "a command or commit passed the blocked guard")
    controller.requestDequeue(1, 1)
    controller.requestDequeue(2, 2)
    dut.clockDomain.waitSampling(16)
    assert(pops.sorted == Seq(1, 2), s"unrelated traffic stopped: $pops")
    assert(disabledBrains == 0 && !cleaned, "wrong PE/FIFO drain released the guard")
    controller.requestDequeue(2, 1)
    cleanup.join()
    assert(pops == Seq(1, 2, 1), s"cleanup corrupted the final old post-mapper lookup: $pops")
    assert(disabledBrains == 1 && dut.io.commitReady.toBoolean)
    assert(dut.io.commitEpoch.toBigInt == epoch + 1)
    assert(accepted.sliding(2).forall(pair => pair(1) - pair(0) >= 10), "multiple instructions accepted per cycle")
    assert(accepted.sliding(2).exists(pair => pair(1) - pair(0) == 10), "lost one-instruction/cycle ingress")

    // Both banks must now contain the invalid enqueue mapping, even after a
    // further commit swaps them again. No special cleanup hardware is involved.
    def observeClearedMapping(): Unit = {
      var mapped = -1
      val observer = fork {
        val request = dut.pifoEngines(1).enque.brain.io.request
        dut.clockDomain.waitSamplingWhere(request.valid.toBoolean)
        mapped = request.payload.vpifoId.toInt
      }
      controller.enqueueToEngine(2, 1)
      observer.join()
      assert(mapped == 0, s"retired pre-map survived cleanup: $mapped")
      dut.clockDomain.waitSampling(10)
    }
    observeClearedMapping()
    send(ControlCommand.CommitMapper, 1)
    observeClearedMapping()

    // Empty at reset, event before guard arrival, and a refilled FIFO.
    send(ControlCommand.GuardDrain, 2, 3)
    send(ControlCommand.CommitMapper, 1)
    controller.enqueueToEngine(2, 3)
    dut.clockDomain.waitSampling(16)
    controller.requestDequeue(2, 3)
    dut.clockDomain.waitSampling(16)
    send(ControlCommand.GuardDrain, 2, 3)
    send(ControlCommand.CommitMapper, 1)
    controller.enqueueToEngine(2, 3)
    dut.clockDomain.waitSampling(16)
    var reusedFinished = false
    val reused = fork {
      send(ControlCommand.GuardDrain, 2, 3)
      send(ControlCommand.CommitMapper, 1)
      reusedFinished = true
    }
    dut.clockDomain.waitSampling(16)
    assert(!reusedFinished, "stale drain released a guard after FIFO refill")
    controller.requestDequeue(2, 3)
    reused.join()
    println("[DrainGuardSim] PASS: tagged blocking, ordinary cleanup, final pop, both banks, early/empty/reused guards")
    simSuccess()
  }
}
