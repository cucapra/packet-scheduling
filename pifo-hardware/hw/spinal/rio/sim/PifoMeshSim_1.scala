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
      val engine1 = 1
      val engine2 = 2

      val tree1 = TreeController(
        controller,
        pifos = Seq((engine1, rPifo(0)), (engine2, rPifo(1)), (engine2, rPifo(2)))
        // engine1 has pifo0, and engine2 has pifo1 and pifo2.
      )

      val configThread = controller.config { cf =>
        cf.tree(tree1)
          .addFlow(rFlow(0), Seq(rPifo(0), rPifo(1)))
          .addFlow(rFlow(1), Seq(rPifo(0), rPifo(2)))
          .brainWFQ(rPifo(0))
          .brainState(rPifo(0), rFlow(0), 1) // weight of flow0 in pifo0 -> 1
          .brainState(rPifo(0), rFlow(1), 1) // weight of flow1 in pifo0 -> 1
          // In pifo0 we are running WFQ with weights 1 and 1, so we are essentially running RR
          .brainFIFO(rPifo(1))
          .brainFIFO(rPifo(2)) // pifo1 and pifo2 run the FIFO policy
      }

      configThread.join()

      println("=== PifoMesh Simulation: Multi-Engine Test ===")
      dut.clockDomain.waitRisingEdge(4)
      // This creates a clear gap in the waveform.
      // It's good to LEAVE this here, since we want to give some time for the config to happen (above).
      // Does a more complicated tree config require a longer wait? NO.

      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }
      // We have enqueued 20 items into the tree.

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 20) {
        tree1.deque
      }
      // we dequeue precisely 20 times, so the tree should be empty again

      controller
        .config { cf =>
          cf.tree(tree1)
            .brainState(rPifo(0), rFlow(0), 2)
        }
        .join()
      // We have NOW changed the brain of pifo0 to give flow0 the weight 2
      // This is immediate, once we use `join()` to join it.

      // the below is just a repetition of the same experiment with the new weights
      // enqueue 20, dequeue 20

      for (i <- 0 until 10) {
        controller.enque(rFlow(0))
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 20) {
        tree1.deque
      }

      println("=== PifoMesh Simulation Completed ===")
    }
}
