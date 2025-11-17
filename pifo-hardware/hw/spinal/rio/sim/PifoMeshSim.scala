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
    .compile(new PifoMesh(testConfig))
    .doSim { dut =>
      def mkFlowId(engineId: Int, vPifoId: Int): Int = {
        assert(engineId >= 0 && engineId <= testConfig.numEngines, s"Invalid engineId: $engineId")
        assert(vPifoId >= 0 && vPifoId < testConfig.numVPIFOs, s"Invalid vPifoId: $vPifoId")

        (engineId << testConfig.vpifoIdWidth) | vPifoId
      }

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
            if (dut.io.pop.valid.toBoolean) {
              val eng = dut.io.pop.payload.engineId.toLong
              val vp = dut.io.pop.payload.vPifoId.toLong
              println(s"[Monitor] Pop response: vPifoId=0x${vp.toHexString}")
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

      val flow0 = 0xe
      val flow1 = 0xf

      val vPifo_A = 0xa
      val vPifo_B = 0xb
      val vPifo_C = 0xc

      // These are not truly magic numbers; we just need some index and we are
      // using these so that they're easy to find in Surfer later on

      val engine_out = 0 // Port 0 is a magic number: it represents a dequeue from the tree
      val engine1 = 1
      val engine2 = 2

      val vPifoMap = Map(
        engine1 -> Map(
          flow0 -> vPifo_A,
          flow1 -> vPifo_A
        ),
        engine2 -> Map(
          flow0 -> vPifo_B,
          flow1 -> vPifo_C
        )
      ) // This map will be helpul later: it will let us easily craft enqueue and dequeue mappers

      println("=== PifoMesh Simulation: Multi-Engine Test ===")

      // Configure engine 1
      println(s"Configuring Engine $engine1...")
      // Format: data = engineType (1=WFQ,2=SP,3=FIFO), vPifoId= vPifoId
      sendControl(ControlCommand.UpdateBrainEngine, engine1, 2, vPifoId = vPifo_A)
      // Engine 1 has a new vPIFO, A, and A has policy SP.

      // add the non-exist rewrite
      // Format: data = (targetEngine, targetvPifoId), vPifoId = vPifoId
      sendControl(
        ControlCommand.UpdateMapperNonExist,
        engine1, // AM: questions about the use of this technology
        mkFlowId(0, testConfig.numVPIFOs - 1),
        vPifoId = vPifo_A
      )
      // SP MUST set per-flow state as priority
      // Format: data = state (priority in SP, Weight in WFQ), flowId = (engineId, flowId), vPifoId = vPifoId
      sendControl(
        ControlCommand.UpdateBrainFlowState,
        engine1,
        10,
        vPifoId = vPifo_A,
        flowId = mkFlowId(engine1, flow0)
      ) // In engine 1 we maintain some state for vPIFO A and flow 0. That state is: 10
      // It is upto vPIFO A how to interpret that state
      sendControl(
        ControlCommand.UpdateBrainFlowState,
        engine1,
        20,
        vPifoId = vPifo_A,
        flowId = mkFlowId(engine1, flow1)
      ) // In engine 1 we maintain some state for vPIFO A and flow 1. That state is: 20
      // It is upto vPIFO A how to interpret that state

      vPifoMap(engine1).foreach { case (flowId, vPifoId) =>
        // Format: data = vPifoId, vPifoId = flowId
        sendControl(ControlCommand.UpdateMapperPre, engine1, vPifoId, vPifoId = flowId) // trivial
        // Format: data = (targetEngineId, targetvPifoId), flowId = (sourceEngineId, sourceFlowId)
        sendControl(
          ControlCommand.UpdateMapperPost,
          engine1,
          mkFlowId(engine2, vPifoMap(engine2)(flowId)),
          // every pop from engine 1 will trigger some action in engine 2
          // AM question: where and how do we customize that action?
          flowId = mkFlowId(engine1, flowId)
        )
      }

      println(s"Configuring Engine $engine2...")
      sendControl(ControlCommand.UpdateBrainEngine, engine2, 3, vPifoId = vPifo_B) // FIFO
      // Should not have non-exist on a non-root vPifo
      sendControl(ControlCommand.UpdateBrainEngine, engine2, 3, vPifoId = vPifo_C) // FIFO
      // We have associated Engine 2 with two vPIFOs, B and C. Those both have policy FIFO
      // This policy requires no per-flow state

      vPifoMap(engine2).foreach { case (flowId, vPifoId) =>
        sendControl(ControlCommand.UpdateMapperPre, engine2, vPifoId, vPifoId = flowId) // trivial
        sendControl(
          ControlCommand.UpdateMapperPost,
          engine2,
          mkFlowId(engine_out, flowId), // report pops to engine 0, which prints etc.
          flowId = mkFlowId(engine2, flowId)
        )
      }

      dut.clockDomain.waitRisingEdge(4)

      println(s"Enqueueing packets to Engine $engine1")
      for (i <- 0 until 3) {
        enqueueToEngine(engine1, flow0) // par 1
        enqueueToEngine(engine2, flow0) // par 1
        dut.clockDomain.waitRisingEdge(1)
        enqueueToEngine(engine1, flow1) // par 2
        enqueueToEngine(engine2, flow1) // par 2
        dut.clockDomain.waitRisingEdge(1)
      }

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue from Engine $engine1 (root vPifo=$vPifo_A):")
      for (_ <- 0 until 8) {
        requestDequeue(engine1, vPifo_A) // not par, and only called on the root
      }

      dut.clockDomain.waitRisingEdge(20)

      println("=== PifoMesh Simulation Completed ===")
    }
}
