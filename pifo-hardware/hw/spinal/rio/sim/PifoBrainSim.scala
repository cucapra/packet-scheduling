package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._

object PifoBrainSim extends App {

  val testConfig = EngineConfig(
    numEngines = 2,
    numVPIFOs = 1024,
    maxPacketPriority = 64,
    fifoDepth = 1024,
    prefetchBufferDepth = 2
  )

  SimConfig
    .withIVerilog
    .addSimulatorFlag("-g2012")
    .withFstWave
    .compile(new PIFOBrain(testConfig)).doSim { dut =>

    def initialize() = {
      dut.io.request.valid #= false
      dut.io.request.payload.vpifoId #= 0
      dut.io.request.payload.flowId #= 0

      dut.io.control.valid #= false
      dut.io.control.payload.command #= ControlCommand.UpdateBrainEngine
      dut.io.control.payload.engineId #= 0
      dut.io.control.payload.vPifoId #= 0
      dut.io.control.payload.flowId #= 0
      dut.io.control.payload.data #= 0

      // poped is a Flow (no valid), clear exist
      dut.io.poped.exist #= false
      dut.io.poped.priority #= 0
      dut.io.poped.port #= 0

      dut.io.response.ready #= true
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

    def sendIn(vp: Int, flow: Int) = {
      dut.io.request.valid #= true
      dut.io.request.payload.vpifoId #= vp
      dut.io.request.payload.flowId #= flow
      dut.clockDomain.waitRisingEdge()
      dut.io.request.valid #= false
    }

    def sendPoped(exist: Boolean, priority: Int, port: Int) = {
      dut.io.poped.exist #= exist
      dut.io.poped.priority #= priority
      dut.io.poped.port #= port
      // one cycle to let writeReq translate
      dut.clockDomain.waitRisingEdge()
      // clear
      dut.io.poped.exist #= false
    }

    def readOutOnce(timeoutCycles: Int = 10) = {
      var seen = false
      dut.io.response.ready #= true
      for (_ <- 0 until timeoutCycles if !seen) {
        dut.clockDomain.waitRisingEdge()
        if (dut.io.response.valid.toBoolean) {
          val pr = dut.io.response.payload.priority.toLong
          val pt = dut.io.response.payload.port.toLong
          val dt = dut.io.response.payload.data.toLong
          println(s"  OUT: port=$pt priority=$pr data=$dt")
          seen = true
        }
      }
      dut.io.response.ready #= false
      seen
    }

    // clock
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
    dut.clockDomain.waitRisingEdge(16)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitRisingEdge(4)

    val testVPifoId = 12
    val testFlowId  = 14

    println("=== PIFOBrain Simulation: FIFO test ===")
    // 2 for WFQ?
    sendControl(ControlCommand.UpdateBrainEngine,    0, 1, vPifoId = testVPifoId)
    sendControl(ControlCommand.UpdateBrainFlowState, 0, 1, vPifoId = testVPifoId, flowId = testFlowId)
    sendControl(ControlCommand.UpdateBrainState,     0, 2, vPifoId = testVPifoId, flowId = testFlowId)

    dut.clockDomain.waitRisingEdge(4)

    sendIn(testVPifoId, testFlowId)
    dut.clockDomain.waitRisingEdge(1)
    sendIn(testVPifoId, testFlowId)
    dut.clockDomain.waitRisingEdge(1)
    sendIn(testVPifoId, testFlowId)
    dut.clockDomain.waitRisingEdge(20)
    // readOutOnce()
    // sendIn(0, 2)
    // readOutOnce()

    // dut.clockDomain.waitRisingEdge(4)

    // println("=== PIFOBrain Simulation: WFQ test ===")

    // // set vPifo 1 to WFQ (encoding 1)
    // sendControl(ControlCommand.UpdateBrainEngine, 1, 1, 0)
    // // set flowState (engineCAM) for flow pifoId@@flowId; choose flowId=3, pifoId=1
    // val pifoId = 1
    // val flowId = 3
    // // set last finish (flow state) to 4
    // sendControl(ControlCommand.UpdateBrainFlowState, 0, pifoId, flowId, 4)
    // // set virtual time via poped for pifoId=1 to 20
    // sendPoped(true, 20, pifoId)

    // // send input for pifoId=1, flowId=3
    // sendIn(pifoId, flowId)
    // // expected newTime = max(virtualTime 20, lastFinish 4) + 16 = 36
    // readOutOnce()

    // dut.clockDomain.waitRisingEdge(4)

    // println("=== PIFOBrain Simulation Completed ===")
  }
}
