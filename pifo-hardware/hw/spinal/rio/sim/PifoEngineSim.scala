package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._

object PifoEngineSim extends App {

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
    .compile(new PifoEngine(testConfig)).doSim { dut =>

    def initialize() = {
      dut.io.enqueRequest.valid #= false
      dut.io.enqueRequest.payload.engineId #= 0
      dut.io.enqueRequest.payload.vPifoId #= 0

      dut.io.dequeueRequest.valid #= false
      dut.io.dequeueRequest.payload.engineId #= 0
      dut.io.dequeueRequest.payload.vPifoId #= 0

      dut.io.control.valid #= false
      dut.io.control.payload.command #= ControlCommand.UpdateMapperPre
      dut.io.control.payload.engineId #= 0
      dut.io.control.payload.vPifoId #= 0
      dut.io.control.payload.flowId #= 0
      dut.io.control.payload.data #= 0

      dut.io.dequeueResponse.ready #= true
    }

    def sendControl(cmd: ControlCommand.E, engineId: Int, data: Int, vPifoId: Int = 0, flowId: Int = 0) = {
      dut.io.control.valid #= true
      dut.io.control.payload.command #= cmd
      dut.io.control.payload.engineId #= engineId
      dut.io.control.payload.vPifoId #= vPifoId
      dut.io.control.payload.flowId #= flowId
      dut.io.control.payload.data #= data
      dut.clockDomain.waitRisingEdge()
      dut.io.control.valid #= false
    }

    def enqueue(engineId: Int, vPifoId: Int) = {
      dut.io.enqueRequest.valid #= true
      dut.io.enqueRequest.payload.engineId #= engineId
      dut.io.enqueRequest.payload.vPifoId #= vPifoId
      dut.clockDomain.waitRisingEdge()
      dut.io.enqueRequest.valid #= false
    }

    def dequeue(vPifoId: Int) = {
      dut.io.dequeueRequest.valid #= true
      dut.io.dequeueRequest.payload.engineId #= 0
      dut.io.dequeueRequest.payload.vPifoId #= vPifoId
      dut.clockDomain.waitRisingEdge()
      dut.io.dequeueRequest.valid #= false
    }

    def monitor() = {
      fork {
        while (true) {
          dut.clockDomain.waitRisingEdge()
          if(dut.io.dequeueResponse.valid.toBoolean) {
            val eng = dut.io.dequeueResponse.payload.engineId.toLong
            val vp = dut.io.dequeueResponse.payload.vPifoId.toLong
            println(s"[Monitor] Dequeue response valid: engineId=$eng, vPifoId=$vp")
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

    val testVPifoId = 12
    val testFlowId  = 14

    println("=== PifoEngine Simulation: FIFO Test ===")

    val testEngineId = 1

    // create id mapping
    sendControl(ControlCommand.UpdateMapperPre, testEngineId, testVPifoId, vPifoId = testVPifoId)
    sendControl(ControlCommand.UpdateMapperPre, testEngineId, testVPifoId + 1, vPifoId = testVPifoId + 1)

    // Configure deque mapper: 0 for sending back to output
    sendControl(ControlCommand.UpdateMapperPost, testEngineId, testVPifoId, flowId = testVPifoId)
    sendControl(ControlCommand.UpdateMapperPost, testEngineId, testVPifoId + 1, flowId = testVPifoId + 1)

    // Configure brain: vPifo 12 -> WFQ, vPifo 13 -> FIFO
    // BrainType encoding: NOP=0, WFQ=1, SP=2, FIFO=3
    sendControl(ControlCommand.UpdateBrainEngine, testEngineId, 1, testVPifoId)
    sendControl(ControlCommand.UpdateBrainEngine, testEngineId, 3, testVPifoId + 1)

    // Initialize brain and flow state
    sendControl(ControlCommand.UpdateBrainState, testEngineId, 2, testVPifoId)
    sendControl(ControlCommand.UpdateBrainFlowState, testEngineId, 1, testVPifoId, testFlowId)

    sendControl(ControlCommand.UpdateBrainState, testEngineId, 33, testVPifoId + 1)
    sendControl(ControlCommand.UpdateBrainFlowState, testEngineId, 33, testVPifoId + 1, testFlowId)

    dut.clockDomain.waitRisingEdge(4)

    println(s"Enqueueing packets: vPifo=$testVPifoId flowId=$testFlowId and vPifo=${testVPifoId + 1} flowId=$testFlowId")
    for (i <- 0 until 5) {
      enqueue(0, testVPifoId)
      dut.clockDomain.waitRisingEdge(1)
      enqueue(0, testVPifoId + 1)
      dut.clockDomain.waitRisingEdge(1)
    }

    dut.clockDomain.waitRisingEdge(6)

    println(s"Dequeuing from vPifo $testVPifoId:")
    for (_ <- 0 until 6) {
      dequeue(testVPifoId)
    }

    println(s"Dequeuing from vPifo ${testVPifoId + 1}:")
    for (_ <- 0 until 6) {
      dequeue(testVPifoId + 1)
    }

    dut.clockDomain.waitRisingEdge(8)

    println("=== PifoEngine Simulation Completed ===")
  }
}
