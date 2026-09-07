package rio.sim

import spinal.core.sim._

import rio._

/** RTL checks for the stop-the-world SP barrier and synthetic-token prefill.
  *
  * Run from pifo-hardware with:
  *   sbt "runMain rio.sim.StopTheWorldPopSim"
  */
object StopTheWorldPopSim extends App {
  private val config = EngineConfig(
    numEngines = 2,
    numVPIFOs = 8,
    maxPacketPriority = 64,
    fifoDepth = 8,
    prefetchBufferDepth = 2
  )
  private val oldRootEngine = 1
  private val oldRootPifo = 1
  private val barrierEngine = 2
  private val barrierPifo = 2
  private val preloadFlow = config.numVPIFOs - 1
  private val packedPreloadFlow =
    (barrierEngine << config.vpifoIdWidth) | preloadFlow
  private val terminalFlow = 1
  private val packedOldFlow =
    (oldRootEngine << config.vpifoIdWidth) | terminalFlow
  private val prefillCount = 3

  SimConfig.withIVerilog
    .addSimulatorFlag("-g2012")
    .compile {
      val mesh = PifoMesh(config)
      mesh.trafficStopped.simPublic()
      mesh.activeRootValid.simPublic()
      mesh.activeRootEngine.simPublic()
      mesh.activeRootPifo.simPublic()
      mesh.pifoEngines(oldRootEngine - 1).prefill.busy.simPublic()
      mesh.pifoEngines(barrierEngine - 1).prefill.busy.simPublic()
      mesh.pifoEngines(barrierEngine - 1).pifos.io.push2.valid.simPublic()
      mesh.pifoEngines(barrierEngine - 1).pifos.io.push2Ready.simPublic()
      mesh.pifoEngines(barrierEngine - 1).pifos.io.push2.priority.simPublic()
      mesh
    }
    .doSim { dut =>
      val controller = PifoMeshSimController(config, dut)
      controller.start(
        enableControlSocket = false,
        controlSocketPath = SimUtils.DefaultControlSocketPath,
        monitorPops = false
      )

      // Seed the old root with three physical tokens. The later data=0 barrier
      // prefill must derive this count from hardware occupancy.
      controller.sendControl(
        ControlCommand.PrefillPifo,
        engineId = oldRootEngine,
        data = prefillCount,
        vPifoId = oldRootPifo,
        flowId = packedOldFlow
      )
      dut.clockDomain.waitSamplingWhere(
        dut.pifoEngines(oldRootEngine - 1).prefill.busy.toBoolean
      )
      dut.clockDomain.waitSamplingWhere(
        !dut.pifoEngines(oldRootEngine - 1).prefill.busy.toBoolean
      )

      controller.sendControl(
        ControlCommand.StopWorld,
        engineId = oldRootEngine,
        data = 0,
        vPifoId = oldRootPifo
      )
      dut.clockDomain.waitSamplingWhere(dut.trafficStopped.toBoolean)

      // The pause is an RTL ready/valid gate, not a simulator-only admission
      // convention. Both packet-token insertion and root pops backpressure.
      dut.io.insert(0).valid #= true
      dut.io.insert(0).payload.engineId #= oldRootEngine
      dut.io.insert(0).payload.vPifoId #= terminalFlow
      dut.io.dataRequest.valid #= true
      dut.io.dataRequest.payload.engineId #= oldRootEngine
      dut.io.dataRequest.payload.vPifoId #= oldRootPifo
      dut.clockDomain.waitSampling(2)
      assert(!dut.io.insert(0).ready.toBoolean)
      assert(!dut.io.dataRequest.ready.toBoolean)
      dut.io.insert(0).valid #= false
      dut.io.dataRequest.valid #= false

      controller.sendControl(
        ControlCommand.UpdateBrainEngine,
        engineId = barrierEngine,
        data = 2,
        vPifoId = barrierPifo
      )
      controller.sendControl(
        ControlCommand.UpdateMapperPost,
        engineId = oldRootEngine,
        data = controller.mkFlowId(0, terminalFlow),
        vPifoId = oldRootPifo,
        flowId = packedOldFlow
      )
      controller.sendControl(
        ControlCommand.UpdateMapperPost,
        engineId = barrierEngine,
        data = controller.mkFlowId(oldRootEngine, oldRootPifo),
        vPifoId = barrierPifo,
        flowId = packedPreloadFlow
      )

      var insertedTokens = 0
      var monitor = true
      val prefillObserver = fork {
        while (monitor) {
          dut.clockDomain.waitSampling()
          val pifos = dut.pifoEngines(barrierEngine - 1).pifos.io
          if (pifos.push2.valid.toBoolean && pifos.push2Ready.toBoolean) {
            assert(pifos.push2.priority.toInt == 1)
            insertedTokens += 1
          }
        }
      }

      controller.sendControl(
        ControlCommand.PrefillPifo,
        engineId = barrierEngine,
        data = 0,
        vPifoId = barrierPifo,
        flowId = packedPreloadFlow
      )
      controller.sendControl(
        ControlCommand.UpdateRoot,
        engineId = barrierEngine,
        data = 0,
        vPifoId = barrierPifo
      )
      controller.sendControl(ControlCommand.CommitMapper, engineId = 1, data = 0)

      monitor = false
      prefillObserver.join()
      assert(insertedTokens == prefillCount, s"expected $prefillCount prefill tokens, got $insertedTokens")
      assert(!dut.trafficStopped.toBoolean)
      assert(dut.activeRootValid.toBoolean)
      assert(dut.activeRootEngine.toInt == barrierEngine)
      assert(dut.activeRootPifo.toInt == barrierPifo)

      // Requests still naming the old root are selected through the committed
      // hardware root register. Every synthetic token is a scheduler entry and
      // therefore produces no simulator-side packet admission.
      (0 until prefillCount).foreach { _ =>
        controller.requestDequeue(oldRootEngine, oldRootPifo)
        dut.clockDomain.waitSamplingWhere(dut.io.pop.valid.toBoolean)
        assert(dut.io.pop.payload.engineId.toInt == 0)
        assert(dut.io.pop.payload.vPifoId.toInt == terminalFlow)
      }

      simSuccess()
    }
}
