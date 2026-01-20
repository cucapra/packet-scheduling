package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._
import spinal.sim.SimThread

object PifoMeshSim_3 extends App {
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
          .brainState(rPifo(0), rFlow(0), 3) // weight of flow0 in pifo0 -> 3
          .brainState(rPifo(0), rFlow(1), 7) // weight of flow1 in pifo0 -> 7
          // In engine 1, we are running WFQ with weights 3 and 7
          // so essentially flow0:flow1 :: 30:70.
          .brainFIFO(rPifo(1))
          .brainFIFO(rPifo(2))
      }

      configThread.join()

      println("=== PifoMesh Simulation: Multi-Engine Test ===")
      dut.clockDomain.waitRisingEdge(4)

      println(s"Enqueueing packets to Engine $engine1")
      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }

      dut.clockDomain.waitRisingEdge(6)

      // Note, we have done 10 pushes and 0 pops.
      // Two ramifications:
      // - We have not tested Zhiyuan's underflow mechanism
      // - We HAVE left 10 elements in the tree, and now it will be interesting to see how the first few dequeues look when we do them later on!

      dut.clockDomain.waitRisingEdge(20)

      // In engine 1, we were running WFQ with weights 3 and 7
      // so essentially flow0:flow1 :: 30:70.
      // Now we are changing it to 70:30.
      controller.config { cf =>
        cf.tree(tree1)
          .brainState(rPifo(0), rFlow(0), 7)
          .brainState(rPifo(0), rFlow(1), 3)
      }

      // Let's again push and pop some packets to appreciate this change.

      dut.clockDomain.waitRisingEdge(4)

      println(s"Enqueueing packets to Engine $engine1")
      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 40) {
        tree1.deque
      }

      dut.clockDomain.waitRisingEdge(20)

      println("=== PifoMesh Simulation Completed ===")
    }
}
