package rio.sim

import rio._
import spinal.core.sim._
import scala.collection.mutable

/** Exercise the mesh adapters with a 16-stage CAM, including long stalls,
  * dependent WFQ/FIFO ranks, controller/write arbitration and PIFO underflows.
  * The small 4-entry chunks reproduce the longest sweep latency in simulation.
  */
object PipelinedCamMeshSim extends App {
  val base = EngineConfig(1, 32, 256, 2, 2, lookupBackend = "cam-pipelined",
    camEntriesPerPe = 64, camEntriesPerStage = 4)
  SimUtils.RioSimConfig.compile(PIFOBrain(base)).doSim { dut =>
    SimTimeout(3000000)
    dut.clockDomain.forkStimulus(10)
    dut.io.request.valid #= false
    dut.io.request.vpifoId #= 0
    dut.io.request.flowId #= 1
    dut.io.response.ready #= false
    dut.io.control.valid #= false
    dut.io.control.command #= ControlCommand.UpdateBrainEngine
    dut.io.control.engineId #= 1
    dut.io.control.vPifoId #= 0
    dut.io.control.flowId #= 1
    dut.io.control.data #= 0
    dut.io.poped.valid #= false
    dut.io.poped.exist #= false
    dut.io.poped.port #= 0
    dut.io.poped.priority #= 0
    dut.io.poped.data #= 0
    dut.clockDomain.assertReset()
    dut.clockDomain.waitSampling(4)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitFallingEdge()
    def control(command: ControlCommand.E, port: Int, value: Int): Unit = {
      dut.io.control.valid #= true
      dut.io.control.command #= command
      dut.io.control.vPifoId #= port
      dut.io.control.data #= value
      dut.clockDomain.waitSamplingWhere(dut.io.control.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
      dut.io.control.valid #= false
    }
    for (port <- 1 to 24) {
      control(ControlCommand.UpdateBrainEngine, port, if (port == 1) 1 else if (port == 2) 3 else 2)
      control(ControlCommand.UpdateBrainFlowState, port, port + 16)
    }
    val expected = mutable.Queue[(Int, Int, Int)]()
    val ranks = mutable.Map(1 -> 17, 2 -> 0)
    val requests = (0 until 160).map(i => if (i % 5 == 0) 1 else if (i % 5 == 1) 2 else 3 + i % 22)
    var received, accepted, stalls, cycles = 0
    var held: Option[(Int, Int, Int)] = None
    var running = true
    val monitor = fork {
      while (running) {
        dut.clockDomain.waitSampling()
        cycles += 1
        if (dut.io.request.valid.toBoolean && dut.io.request.ready.toBoolean) {
          val port = dut.io.request.vpifoId.toInt
          val rank = if (port == 1) { ranks(1) = (ranks(1) + 16) & 255; ranks(1) }
            else if (port == 2) { ranks(2) += 1; ranks(2) } else port + 16
          if (rank != 0) expected.enqueue((port, 1, rank))
          accepted += 1
        }
        val valid = dut.io.response.valid.toBoolean
        val value = (dut.io.response.port.toInt, dut.io.response.data.toInt, dut.io.response.priority.toInt)
        held.foreach(previous => assert(valid && value == previous, "brain output changed under backpressure"))
        held = if (valid && !dut.io.response.ready.toBoolean) Some(value) else None
        if (valid && dut.io.response.ready.toBoolean) {
          assert(expected.nonEmpty, s"unexpected brain response $value")
          assert(value == expected.dequeue(), s"misaligned or stale rank response $value")
          received += 1
        }
        if (dut.io.request.valid.toBoolean && !dut.io.request.ready.toBoolean) stalls += 1
        dut.clockDomain.waitFallingEdge()
        dut.io.response.ready #= (cycles % 43 >= 25)
      }
    }
    val concurrentControl = fork {
      dut.clockDomain.waitSampling(33)
      dut.clockDomain.waitFallingEdge()
      // An unrelated control write may contend with a packet-driven rank write.
      for (value <- 1 to 4) control(ControlCommand.UpdateBrainFlowState, 31, value)
    }
    for (port <- requests) {
      dut.io.request.valid #= true
      dut.io.request.vpifoId #= port
      dut.clockDomain.waitSamplingWhere(dut.io.request.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
    }
    dut.io.request.valid #= false
    concurrentControl.join()
    dut.clockDomain.waitSamplingWhere(accepted == requests.size && expected.isEmpty)
    dut.clockDomain.waitSampling(100)
    running = false
    monitor.join()
    assert(stalls > 0 && received > 100 && !dut.io.camCapacityBlocked.toBoolean)
    println(s"PIPELINED_BRAIN_BURST_PASS stages=16 accepted=$accepted responses=$received stalls=$stalls dependent_ranks=true")
    simSuccess()
  }

  for (replay <- Seq(false, true)) {
    val config = base.copy(pifoBackend = "external", dynamicConfig = replay)
    SimUtils.RioSimConfig.compile(PifoEngine(config)).doSim { dut =>
      SimTimeout(3000000)
      dut.clockDomain.forkStimulus(10)
      dut.io.enqueRequest.valid #= false
      dut.io.enqueRequest.engineId #= 1
      dut.io.enqueRequest.vPifoId #= 1
      dut.io.dequeueRequest.valid #= false
      dut.io.dequeueRequest.engineId #= 1
      dut.io.dequeueRequest.vPifoId #= 1
      dut.io.dequeueResponse.ready #= false
      dut.io.control.valid #= false
      dut.io.control.command #= ControlCommand.UpdateMapperPost
      dut.io.control.engineId #= 1
      dut.io.control.vPifoId #= 1
      dut.io.control.flowId #= 1
      dut.io.control.data #= 0
      dut.io.pifo.popResponse.valid #= false
      dut.io.pifo.popResponse.exist #= false
      dut.io.pifo.popResponse.port #= 0
      dut.io.pifo.popResponse.data #= 0
      dut.io.pifo.popResponse.priority #= 0
      dut.io.pifo.popPortEmpty #= false
      dut.io.pifo.portDrained.valid #= false
      dut.io.pifo.portDrained.payload #= 0
      dut.io.pifo.portPushed.foreach { p => p.valid #= false; p.payload #= 0 }
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitFallingEdge()
      def control(command: ControlCommand.E, port: Int, value: Int): Unit = {
        dut.io.control.valid #= true
        dut.io.control.command #= command
        dut.io.control.vPifoId #= port
        dut.io.control.data #= value
        dut.clockDomain.waitSamplingWhere(dut.io.control.ready.toBoolean)
        dut.clockDomain.waitFallingEdge()
        dut.io.control.valid #= false
      }
      def target(port: Int) = 32 | ((port + 3) % 32)
      for (port <- 1 to 24) control(ControlCommand.UpdateMapperPost, port, target(port))
      control(ControlCommand.CommitMapper, 0, 0)
      val expected = mutable.Queue[Int]()
      var issued, received, underflows, stalls, cycles = 0
      var held: Option[Int] = None
      var running = true
      val model = fork {
        while (running) {
          dut.clockDomain.waitSampling()
          cycles += 1
          val requested = dut.io.pifo.popRequest.valid.toBoolean
          val port = dut.io.pifo.popRequest.port.toInt
          val exists = issued % 7 != 0
          if (requested) {
            issued += 1
            if (exists) expected.enqueue(target(port)) else underflows += 1
          }
          val valid = dut.io.dequeueResponse.valid.toBoolean
          val value = (dut.io.dequeueResponse.engineId.toInt << 5) | dut.io.dequeueResponse.vPifoId.toInt
          held.foreach(previous => assert(valid && previous == value, "post-mapper response changed while stalled"))
          held = if (valid && !dut.io.dequeueResponse.ready.toBoolean) Some(value) else None
          if (valid && dut.io.dequeueResponse.ready.toBoolean) {
            assert(expected.nonEmpty && value == expected.dequeue(), "lost, duplicated or misaligned post-mapper response")
            received += 1
          }
          if (dut.io.dequeueRequest.valid.toBoolean && !dut.io.dequeueRequest.ready.toBoolean) stalls += 1
          dut.clockDomain.waitFallingEdge()
          dut.io.pifo.popResponse.valid #= requested
          dut.io.pifo.popResponse.exist #= exists
          dut.io.pifo.popResponse.port #= port
          dut.io.pifo.popResponse.data #= 1
          dut.io.pifo.popResponse.priority #= port
          dut.io.dequeueResponse.ready #= (cycles % 67 >= 48)
        }
      }
      val shadowSync = fork {
        if (replay) {
          dut.clockDomain.waitSampling(20)
          dut.clockDomain.waitFallingEdge()
          for (port <- 1 to 24) control(ControlCommand.UpdateMapperPost, port, target(port))
        }
      }
      for (i <- 0 until 240) {
        dut.io.dequeueRequest.valid #= true
        dut.io.dequeueRequest.vPifoId #= (1 + i % 24)
        dut.clockDomain.waitSamplingWhere(dut.io.dequeueRequest.ready.toBoolean)
        dut.clockDomain.waitFallingEdge()
      }
      dut.io.dequeueRequest.valid #= false
      dut.clockDomain.waitSamplingWhere(received + underflows == 240)
      shadowSync.join()
      dut.clockDomain.waitSampling(100)
      running = false
      model.join()
      assert(issued == 240 && expected.isEmpty && stalls > 0 && underflows > 0)
      assert(!dut.io.camCapacityBlocked.toBoolean)
      println(s"PIPELINED_PIFO_CREDIT_PASS stages=16 replay=$replay requests=$issued responses=$received underflows=$underflows stalls=$stalls")
      simSuccess()
    }
  }
}
