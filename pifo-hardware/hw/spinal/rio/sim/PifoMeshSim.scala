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
    .compile(new PifoMesh(testConfig)).doSim { dut =>

    val controller = PifoMeshSimController(testConfig, dut)

    controller.start

    import RioPredefinedPifos._

    val tree1 = TreeController(
      controller,
      pifos = Seq((1, rPifo(0)), (2, rPifo(1)), (2, rPifo(2)))
    )


    // use controller.transaction if you want transactional configuration
    val configThread = controller.config { cf =>
      cf.tree(tree1)
       .addFlow(rFlow(0), Seq(rPifo(0), rPifo(1)))
       .addFlow(rFlow(1), Seq(rPifo(0), rPifo(2)))

       .brainSP(rPifo(0))
       .brainState(rPifo(0), rFlow(0), 10)  // prio flow0 -> 10
       .brainState(rPifo(0), rFlow(1), 20)  // prio flow1 -> 20

       .brainFIFO(rPifo(1))
       .brainFIFO(rPifo(2))
    }

    // join here to ensure configuration completes before proceeding
    // but you could let it run in parallel with other testbench activity!
    configThread.join()

    println("=== PifoMesh Simulation: Multi-Engine Test ===")
    dut.clockDomain.waitRisingEdge(4)

    for (i <- 0 until 3) {
      controller.enque(rFlow(0))
      dut.clockDomain.waitRisingEdge(1)
      controller.enque(rFlow(1))
      dut.clockDomain.waitRisingEdge(1)
    }

    dut.clockDomain.waitRisingEdge(6)

    println(s"Requesting dequeue (root vPifo=${rPifo(0)}):")
    for (_ <- 0 until 8) {
      tree1.deque
    }

    dut.clockDomain.waitRisingEdge(20)

    println("=== PifoMesh Simulation Completed ===")
  }
}
