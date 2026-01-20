package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._
import spinal.sim.SimThread

object PifoMeshSim_1 extends App {
  val testConfig = EngineConfig(
    numEngines = 2,
    numVPIFOs = 32,
    maxPacketPriority = 256,
    fifoDepth = 32,
    prefetchBufferDepth = 2
  )

  SimConfig.withIVerilog
    .addSimulatorFlag("-g2012")
    .withFstWave
    .compile(new PifoMesh(testConfig))
    .doSim { dut =>
      val controller = PifoMeshSimController(testConfig, dut)

      controller.start

      import RioPredefinedPifos._
      // Once you import this, rPifo(0) is 0xA... rPifo(5) is 0xF.

      val engine1 = 1
      val engine2 = 2

      val tree1 = TreeController(
        controller,
        pifos = Seq((engine1, rPifo(0)), (engine2, rPifo(1)), (engine2, rPifo(2)))
      )

      val configThread = controller.config { cf =>
        cf.tree(tree1)
          .addFlow(rFlow(0), Seq(rPifo(0), rPifo(1)))
          .addFlow(rFlow(1), Seq(rPifo(0), rPifo(2)))
          .brainWFQ(rPifo(0))
          .brainState(rPifo(0), rFlow(0), 1) // weight of flow0 in pifo0 -> 1
          .brainState(rPifo(0), rFlow(1), 1) // weight of flow1 in pifo0 -> 1
          .brainFIFO(rPifo(1))
          .brainFIFO(rPifo(2))
      }

      configThread.join()

      println("=== PifoMesh Simulation: Multi-Engine Test ===")
      dut.clockDomain.waitRisingEdge(4)

      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 20) {
        tree1.deque
      }

      dut.clockDomain.waitRisingEdge(20)

      controller.config { cf =>
        cf.tree(tree1)
          .brainState(rPifo(0), rFlow(0), 2)
      }

      dut.clockDomain.waitRisingEdge(4)

      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 20) {
        tree1.deque
      }

      dut.clockDomain.waitRisingEdge(20)

      println("=== PifoMesh Simulation Completed ===")
    }
}
