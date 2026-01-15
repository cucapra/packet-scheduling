package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._

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

      val flow0 = 0xd
      val flow1 = 0xe
      val flow2 = 0xf

      val vPifo_A = 0xa
      val vPifo_B = 0xb
      val vPifo_C = 0xc
      val vPifo_D = 0xdd

      // These are not truly magic numbers; we just need some index and we are
      // using these so that they're easy to find in Surfer later on

      val engine_out = 0 // Port 0 is a magic number: it represents a dequeue from the tree
      val engine1 = 1
      val engine2 = 2

      val vPifoMap1 = Map(
        engine1 -> Map(
          flow0 -> vPifo_A,
          flow1 -> vPifo_A
          // flow2 -> vPifo_A
        ),
        engine2 -> Map(
          flow0 -> vPifo_B,
          flow1 -> vPifo_C
          // flow2 -> vPifo_D
        )
      ) // for the first policy
      val vPifoMap2 = Map(
        engine1 -> Map(
          flow0 -> vPifo_A,
          flow1 -> vPifo_A,
          flow2 -> vPifo_A
        ),
        engine2 -> Map(
          flow0 -> vPifo_B,
          flow1 -> vPifo_C,
          flow2 -> vPifo_D
        )
      ) // for the second policy

      println("=== PifoMesh Simulation: Multi-Engine Test ===")

      // Configure engine 1
      println(s"Configuring Engine $engine1...")
      // Format: data = engineType (1=WFQ,2=SP,3=FIFO), vPifoId= vPifoId
      sendControl(ControlCommand.UpdateBrainEngine, engine1, 1, vPifoId = vPifo_A)
      // Engine 1 has a new vPIFO, A, and A has policy WFQ.

      // add the non-exist rewrite
      // Format: data = (targetEngine, targetvPifoId), vPifoId = vPifoId
      sendControl(
        ControlCommand.UpdateMapperNonExist,
        engine1,
        mkFlowId(engine_out, testConfig.numVPIFOs - 1), // Engine1 will ping engine_out in case of underflow
        vPifoId = vPifo_A
      )
      // WFQ MUST set per-flow state as priority
      // Format: data = state (priority in SP, Weight in WFQ), flowId = (engineId, flowId), vPifoId = vPifoId
      sendControl(
        ControlCommand.UpdateBrainFlowState,
        engine1,
        1, // that state goes here
        vPifoId = vPifo_A,
        flowId = mkFlowId(engine1, flow0)
      ) // In engine 1 we maintain some state for vPIFO A and flow 0.
      // It is up to vPIFO A how to interpret that state
      sendControl(
        ControlCommand.UpdateBrainFlowState,
        engine1,
        1, // that state goes here
        vPifoId = vPifo_A,
        flowId = mkFlowId(engine1, flow1)
      ) // In engine 1 we maintain some state for vPIFO A and flow 1.
      // It is up to vPIFO A how to interpret that state

      // In engine 1, we are running WFQ with weights 1, and 1
      // so essentially RR b/w flows 0, and 1.
      // There is no flow 2 yet.

      vPifoMap1(engine1).foreach { case (flowId, vPifoId) =>
        // Format: data = vPifoId, vPifoId = flowId
        sendControl(ControlCommand.UpdateMapperPre, engine1, vPifoId, vPifoId = flowId) // trivial
        // Format: data = (targetEngineId, targetvPifoId), flowId = (sourceEngineId, sourceFlowId)
        sendControl(
          ControlCommand.UpdateMapperPost,
          engine1,
          mkFlowId(engine2, vPifoMap1(engine2)(flowId)),
          // every pop from engine 1 will trigger some action in engine 2
          // AM question: where and how do we customize that action?
          flowId = mkFlowId(engine1, flowId)
        )
      }

      println(s"Configuring Engine $engine2...")
      sendControl(ControlCommand.UpdateBrainEngine, engine2, 3, vPifoId = vPifo_B) // 3 = FIFO
      // Should not have non-exist on a non-root vPifo
      sendControl(ControlCommand.UpdateBrainEngine, engine2, 3, vPifoId = vPifo_C) // 3 = FIFO
      sendControl(ControlCommand.UpdateBrainEngine, engine2, 3, vPifoId = vPifo_D) // 3 = FIFO
      // We have associated Engine 2 with three vPIFOs, B, C, and D. Those all have policy FIFO
      // This policy requires no per-flow state, so we skip some of the steps we performed earlier.

      vPifoMap1(engine2).foreach { case (flowId, vPifoId) =>
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
      for (i <- 0 until 10) {
        enqueueToEngine(engine1, flow0) // par block #1
        enqueueToEngine(engine2, flow0) // par block #1
        dut.clockDomain.waitRisingEdge(1)
        enqueueToEngine(engine1, flow1) // par block #2
        enqueueToEngine(engine2, flow1) // par block #2
        dut.clockDomain.waitRisingEdge(1)
        // NOT sending any packets to flow2 yet...
      }

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue from Engine $engine1 (root vPifo=$vPifo_A):")
      for (_ <- 0 until 10) {
        requestDequeue(engine1, vPifo_A) // not par, and only called on the root
      }

      // We do 10 pops. This means that 10 packets are still in the buffer.

      // Now we will tell engines 1 and 2 about flow 2

      sendControl(
        ControlCommand.UpdateBrainFlowState,
        engine1,
        1, // that state goes here
        vPifoId = vPifo_A,
        flowId = mkFlowId(engine1, flow2)
      ) // In engine 1 we maintain some state for vPIFO A and flow 2.
      // It is up to vPIFO A how to interpret that state

      vPifoMap2(engine1).foreach { case (flowId, vPifoId) =>
        // Format: data = vPifoId, vPifoId = flowId
        sendControl(ControlCommand.UpdateMapperPre, engine1, vPifoId, vPifoId = flowId) // trivial
        // Format: data = (targetEngineId, targetvPifoId), flowId = (sourceEngineId, sourceFlowId)
        sendControl(
          ControlCommand.UpdateMapperPost,
          engine1,
          mkFlowId(engine2, vPifoMap2(engine2)(flowId)),
          // every pop from engine 1 will trigger some action in engine 2
          // AM question: where and how do we customize that action?
          flowId = mkFlowId(engine1, flowId)
        )
      }
      // Question for Zhiyuan: may I _just_ send the delta, i.e. flow 2 info? Or must I send it all? For now I am sending pre- and post-mapper info for all the flows again.

      println(s"Configuring Engine $engine2...")
      sendControl(ControlCommand.UpdateBrainEngine, engine2, 3, vPifoId = vPifo_D) // 3 = FIFO
      // We have associated Engine 2 with an additional vPIFO, D. It runs FIFO
      // This policy requires no per-flow state, so we skip some of the steps we performed earlier.

      vPifoMap2(engine2).foreach { case (flowId, vPifoId) =>
        sendControl(ControlCommand.UpdateMapperPre, engine2, vPifoId, vPifoId = flowId) // trivial
        sendControl(
          ControlCommand.UpdateMapperPost,
          engine2,
          mkFlowId(engine_out, flowId), // report pops to engine 0, which prints etc.
          flowId = mkFlowId(engine2, flowId)
        )
      }
      // Same here. May I send fewer pre- and post-mapper messages?

      dut.clockDomain.waitRisingEdge(4)

      println(s"Enqueueing packets to Engine $engine1")
      for (i <- 0 until 10) {
        enqueueToEngine(engine1, flow0) // par block #1
        enqueueToEngine(engine2, flow0) // par block #1
        dut.clockDomain.waitRisingEdge(1)
        enqueueToEngine(engine1, flow1) // par block #2
        enqueueToEngine(engine2, flow1) // par block #2
        dut.clockDomain.waitRisingEdge(1)
        enqueueToEngine(engine1, flow2) // par block #3
        enqueueToEngine(engine2, flow2) // par block #3
        dut.clockDomain.waitRisingEdge(1)
      }

      dut.clockDomain.waitRisingEdge(6)

      println(s"Requesting dequeue from Engine $engine1 (root vPifo=$vPifo_A):")
      for (_ <- 0 until 40) {
        requestDequeue(engine1, vPifo_A) // not par, and only called on the root
      }

      println("=== PifoMesh Simulation Completed ===")
    }
}
