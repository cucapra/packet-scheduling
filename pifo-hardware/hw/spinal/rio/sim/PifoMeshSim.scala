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

    val flow0 = 0xE
    val flow1 = 0xF

    val vPifo_A = 0xA
    val vPifo_B = 0xB
    val vPifo_C = 0xC

    val engine1 = 1
    val engine2 = 2

    val tree1 = TreeController(
      controller,
      pifos = Seq((engine1, vPifo_A), (engine2, vPifo_B), (engine2, vPifo_C))
    )

    val configThread = tree1.async_config { tc => {
      tc.addFlow(flow0, Seq(vPifo_A, vPifo_B))
      tc.addFlow(flow1, Seq(vPifo_A, vPifo_C))

      tc.setBrainSP(vPifo_A)
      tc.setBrainState(vPifo_A, flow0, 10)  // prio flow0 -> 10
      tc.setBrainState(vPifo_A, flow1, 20)  // prio flow1 -> 20
      tc.setBrainFIFO(vPifo_B)
      tc.setBrainFIFO(vPifo_C)
    }}

    // join here to ensure configuration completes before proceeding
    // but you could let it run in parallel with other testbench activity!
    configThread.join()

    println("=== PifoMesh Simulation: Multi-Engine Test ===")
    dut.clockDomain.waitRisingEdge(4)

    println(s"Enqueueing packets to Engine $engine1")
    for (i <- 0 until 3) {
      controller.enque(flow0)
      dut.clockDomain.waitRisingEdge(1)
      controller.enque(flow1)
      dut.clockDomain.waitRisingEdge(1)
    }

    dut.clockDomain.waitRisingEdge(6)

    println(s"Requesting dequeue from Engine $engine1 (root vPifo=$vPifo_A):")
    for (_ <- 0 until 8) {
      tree1.deque
    }

    dut.clockDomain.waitRisingEdge(20)

    println("=== PifoMesh Simulation Completed ===")
  }
}
