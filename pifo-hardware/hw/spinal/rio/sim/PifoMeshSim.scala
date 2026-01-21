package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._
import spinal.sim.SimThread

object PifoMeshSim extends App {
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
      // Once you import this, rPifo(0) is 0xA .. rPifo(5) is 0xF.

      val engine1 = 1
      val engine2 = 2

      val tree1 = TreeController(
        controller,
        pifos = Seq((engine1, rPifo(0)), (engine2, rPifo(1)), (engine2, rPifo(2)))
        // engine1 has pifo0, and engine2 has pifo1 and pifo2.
      )

      // use controller.transaction if you want transactional configuration
      val configThread = controller.config { cf =>
        cf.tree(tree1)
          .addFlow(rFlow(0), Seq(rPifo(0), rPifo(1))) // flow0 is associated with BOTH pifo0 and pifo1
          .addFlow(rFlow(1), Seq(rPifo(0), rPifo(2))) // flow1 is associated with BOTH pifo0 and pifo2
          .brainSP(rPifo(0)) // pifo0 runs the SP policy
          .brainState(rPifo(0), rFlow(0), 10) // prio of flow0 in pifo0 -> 10
          .brainState(rPifo(0), rFlow(1), 20) // prio of flow1 in pifo0 -> 20
          .brainFIFO(rPifo(1))
          .brainFIFO(rPifo(2)) // pifo1 and pifo2 run the FIFO policy
      }

      // join here to ensure configuration completes before proceeding
      // but you could let it run in parallel with other testbench activity!
      configThread.join()

      println("=== PifoMesh Simulation: Multi-Engine Test ===")
      dut.clockDomain.waitRisingEdge(4)

      for (i <- 0 until 3) {
        controller.enque(
          rFlow(0)
        ) // this is a compound function that enqueues into all the pifos associated with the flow.
        // AM question: true?
        // AM question: but it's not in parallel? See Utils.scala.
        dut.clockDomain.waitRisingEdge(1)
        controller.enque(rFlow(1))
        dut.clockDomain.waitRisingEdge(1)
      }
      // So a total of 6 items have been enqueued.

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
      for (_ <- 0 until 8) {
        tree1.deque
      }
      // By doing 8 dequeues we will probably trigger underflow.
      // AM question: true?

      dut.clockDomain.waitRisingEdge(20)

      println("=== PifoMesh Simulation Completed ===")
    }
}
