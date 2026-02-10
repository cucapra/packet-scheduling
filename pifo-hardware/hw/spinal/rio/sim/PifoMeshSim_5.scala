package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._
import spinal.sim.SimThread

object PifoMeshSim_5 extends App {
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
        pifos = Seq((engine1, rPifo(0)), (engine2, rPifo(1)), (engine2, rPifo(2)), (engine2, rPifo(3)))
        // engine1 has pifo0, and engine2 has pifo1, pifo2, and pifo3.
      )

      val configThread = controller.config { cf =>
        cf.tree(tree1)
          .addFlow(rFlow(0), Seq(rPifo(0), rPifo(1)))
          .addFlow(rFlow(1), Seq(rPifo(0), rPifo(2)))
          // flow2 not configured yet
          .brainWFQ(rPifo(0))
          .brainState(rPifo(0), rFlow(0), 1) // weight of flow0 in pifo0 -> 1
          .brainState(rPifo(0), rFlow(1), 1) // weight of flow1 in pifo0 -> 1
          // In pifo0 we are running WFQ with weights 1 and 1
          // so essentially RR b/w flows 0 and 1.
          // There is no flow2 yet.
          .brainFIFO(rPifo(1))
          .brainFIFO(rPifo(2))
          .brainFIFO(rPifo(3)) // pifo1, pifo2, and pifo3 run the FIFO policy
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
        // NOT sending any packets to flow2 yet...
      }
      // We have enqueued 20 items into the tree.

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 10) {
        tree1.deque
      }
      // We dequeue 10 times. This means that 10 packets are still in the tree.

      // Now we will tell the tree about flow2
      controller
        .config { cf =>
          cf.tree(tree1)
            .addFlow(rFlow(2), Seq(rPifo(0), rPifo(3)))
            .brainState(rPifo(0), rFlow(2), 1)
        }
        .join()

      println(s"Enqueueing packets to Engine $engine1")
      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(2))
        dut.clockDomain.waitRisingEdge(1)
      }
      // We have enqueued 30 more items into the tree (40 total now).

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 40) {
        tree1.deque
      }
      // we dequeue precisely 40 times, so the tree should be empty again

      println("=== PifoMesh Simulation Completed ===")
    }
}
