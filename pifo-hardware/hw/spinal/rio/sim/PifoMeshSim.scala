package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._

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

    def initialize() = {
      // Initialize all insert ports
      for (i <- 0 until testConfig.numEngines) {
        dut.io.insert(i).valid #= false
        dut.io.insert(i).payload.engineId #= 0
        dut.io.insert(i).payload.vPifoId #= 0
      }

      // Initialize dataRequest port
      dut.io.dataRequest.valid #= false
      dut.io.dataRequest.payload.engineId #= 0
      dut.io.dataRequest.payload.vPifoId #= 0

      // Initialize pop port
      dut.io.pop.ready #= true

      // Initialize control port
      dut.io.controlRequest.valid #= false
      dut.io.controlRequest.payload.command #= ControlCommand.UpdateMapperPre
      dut.io.controlRequest.payload.engineId #= 0
      dut.io.controlRequest.payload.vPifoId #= 0
      dut.io.controlRequest.payload.flowId #= 0
      dut.io.controlRequest.payload.data #= 0
    }

    def sendControl(cmd: ControlCommand.E, engineId: Int, data: Int, vPifoId: Int = 0, flowId: Int = 0) = {
      dut.io.controlRequest.valid #= true
      dut.io.controlRequest.payload.command #= cmd
      dut.io.controlRequest.payload.engineId #= engineId
      dut.io.controlRequest.payload.vPifoId #= vPifoId
      dut.io.controlRequest.payload.flowId #= flowId
      dut.io.controlRequest.payload.data #= data
      dut.clockDomain.waitRisingEdge()
      dut.io.controlRequest.valid #= false
    }

    def enqueueToEngine(engineId: Int, vPifoId: Int) = {
      val pid = engineId - 1 // Adjust for control port offset
      dut.io.insert(pid).valid #= true
      dut.io.insert(pid).payload.engineId #= engineId
      dut.io.insert(pid).payload.vPifoId #= vPifoId
      dut.clockDomain.waitRisingEdge()
      dut.io.insert(pid).valid #= false
    }

    def requestDequeue(engineId: Int, vPifoId: Int) = {
      dut.io.dataRequest.valid #= true
      dut.io.dataRequest.payload.engineId #= engineId
      dut.io.dataRequest.payload.vPifoId #= vPifoId
      dut.clockDomain.waitRisingEdge()
      dut.io.dataRequest.valid #= false
    }

    def monitor() = {
      fork {
        while (true) {
          dut.clockDomain.waitRisingEdge()
          if(dut.io.pop.valid.toBoolean) {
            val eng = dut.io.pop.payload.engineId.toLong
            val vp = dut.io.pop.payload.vPifoId.toLong
            println(s"[Monitor] Pop response valid: engineId=$eng, vPifoId=$vp")
          }
        }
      }
    }

    // Clock generation
    fork {
      while (true) {
        sleep(5)
        dut.clockDomain.clockToggle()
        sleep(5)
        dut.clockDomain.clockToggle()
      }
    }

    initialize()

    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge(4)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitRisingEdge(4)

    monitor()

    val testVPifoId0 = 12
    val testVPifoId1 = 13
    val testFlowId  = 14

    println("=== PifoMesh Simulation: Multi-Engine Test ===")

    val testEngineId0 = 1
    val testEngineId1 = 2

    // Configure engine 0
    println("Configuring Engine 0...")
    sendControl(ControlCommand.UpdateMapperPre, testEngineId0, testVPifoId0, vPifoId = testVPifoId0)
    sendControl(ControlCommand.UpdateMapperPost, testEngineId0, testVPifoId0, flowId = testVPifoId0)
    sendControl(ControlCommand.UpdateBrainEngine, testEngineId0, 3, testVPifoId0)  // FIFO
    sendControl(ControlCommand.UpdateBrainState, testEngineId0, 2, testVPifoId0)
    sendControl(ControlCommand.UpdateBrainFlowState, testEngineId0, 1, testVPifoId0, testFlowId)

    // Configure engine 1
    println("Configuring Engine 1...")
    sendControl(ControlCommand.UpdateMapperPre, testEngineId1, testVPifoId1, vPifoId = testVPifoId1)
    sendControl(ControlCommand.UpdateMapperPost, testEngineId1, testVPifoId1, flowId = testVPifoId1)
    sendControl(ControlCommand.UpdateBrainEngine, testEngineId1, 1, testVPifoId1)  // WFQ
    sendControl(ControlCommand.UpdateBrainState, testEngineId1, 33, testVPifoId1)
    sendControl(ControlCommand.UpdateBrainFlowState, testEngineId1, 33, testVPifoId1, testFlowId)

    dut.clockDomain.waitRisingEdge(4)

    println(s"Enqueueing packets to Engine 0: vPifo=$testVPifoId0")
    for (i <- 0 until 3) {
      enqueueToEngine(testEngineId0, testVPifoId0)
      dut.clockDomain.waitRisingEdge(1)
    }

    println(s"Enqueueing packets to Engine 1: vPifo=$testVPifoId1")
    for (i <- 0 until 3) {
      enqueueToEngine(testEngineId1, testVPifoId1)
      dut.clockDomain.waitRisingEdge(1)
    }

    dut.clockDomain.waitRisingEdge(6)

    println(s"Requesting dequeue from Engine 0 (vPifo $testVPifoId0):")
    for (_ <- 0 until 4) {
      requestDequeue(testEngineId0, testVPifoId0)
    }

    dut.clockDomain.waitRisingEdge(8)

    println(s"Requesting dequeue from Engine 1 (vPifo $testVPifoId1):")
    for (_ <- 0 until 4) {
      requestDequeue(testEngineId1, testVPifoId1)
    }

    dut.clockDomain.waitRisingEdge(8)

    println("=== PifoMesh Simulation Completed ===")
  }
}
