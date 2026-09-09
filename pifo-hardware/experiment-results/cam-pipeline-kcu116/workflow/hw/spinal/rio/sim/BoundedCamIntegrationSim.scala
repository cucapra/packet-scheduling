package rio.sim

import rio._
import spinal.core.sim._

/** Exercise the pair namespace and packet-driven state writes, then verify that
  * a full CAM backpressures the shared controller before a following commit.
  */
object BoundedCamIntegrationSim extends App {
  val config = EngineConfig(1, 4, 256, 2, 2,
    lookupBackend = args.headOption.getOrElse("cam"), camEntriesPerPe = 2)

  SimUtils.RioSimConfig.compile(PIFOBrain(config)).doSim { dut =>
    SimTimeout(100000)
    dut.clockDomain.forkStimulus(period = 10)
    dut.io.request.valid #= false
    dut.io.request.vpifoId #= 0
    dut.io.request.flowId #= 0
    dut.io.control.valid #= false
    dut.io.control.command #= ControlCommand.UpdateBrainEngine
    dut.io.control.engineId #= 1
    dut.io.control.vPifoId #= 0
    dut.io.control.flowId #= 0
    dut.io.control.data #= 0
    dut.io.poped.valid #= false
    dut.io.poped.exist #= false
    dut.io.poped.port #= 0
    dut.io.poped.priority #= 0
    dut.io.poped.data #= 0
    dut.io.response.ready #= true
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge(4)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitFallingEdge()

    def control(command: ControlCommand.E, port: Int, value: Int, key: Int = 0): Unit = {
      dut.io.control.valid #= true
      dut.io.control.command #= command
      dut.io.control.vPifoId #= port
      dut.io.control.flowId #= key
      dut.io.control.data #= value
      dut.clockDomain.waitSamplingWhere(dut.io.control.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
      dut.io.control.valid #= false
      dut.clockDomain.waitSampling(6)
      dut.clockDomain.waitFallingEdge()
    }
    def lookup(port: Int, key: Int, priority: Int): Unit = {
      var count = 0
      val monitor = fork {
        for (_ <- 0 until 30) {
          dut.clockDomain.waitSampling()
          if (dut.io.response.valid.toBoolean) {
            count += 1
            assert(dut.io.response.port.toInt == port)
            assert(dut.io.response.data.toInt == key)
            assert(dut.io.response.priority.toInt == priority)
          }
        }
      }
      dut.io.request.valid #= true
      dut.io.request.vpifoId #= port
      dut.io.request.flowId #= key
      dut.clockDomain.waitSamplingWhere(dut.io.request.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
      dut.io.request.valid #= false
      monitor.join()
      assert(count == (if (priority == 0) 0 else 1))
      if (config.usesCam) assert(!dut.io.camCapacityBlocked.toBoolean)
      dut.clockDomain.waitFallingEdge()
    }
    import ControlCommand._
    control(UpdateBrainEngine, 1, 2) // strict priority
    control(UpdateBrainEngine, 2, 2)
    control(UpdateBrainFlowState, 1, 37, 2)
    control(UpdateBrainFlowState, 2, 91, 2) // same flow, different vPIFO
    lookup(1, 2, 37)
    lookup(2, 2, 91)
    lookup(1, 3, 0)
    control(UpdateBrainEngine, 1, 1) // WFQ updates the same bounded state table
    control(UpdateBrainFlowState, 1, 6, 2)
    lookup(1, 2, 22)
    lookup(1, 2, 38)
    control(UpdateBrainFlowState, 2, 0, 2) // reclaim before adding a new pair
    control(UpdateBrainFlowState, 1, 77, 3)
    control(UpdateBrainEngine, 1, 2)
    lookup(1, 3, 77)
    lookup(2, 2, 0)
    println(s"CAM_BRAIN_PASS backend=${config.lookupBackend} pair_namespace=true packet_state_updates=true zero_delete=true")
    simSuccess()
  }

  if (config.usesCam) SimUtils.RioSimConfig.compile(PifoMesh(config)).doSim { dut =>
    SimTimeout(100000)
    dut.clockDomain.forkStimulus(period = 10)
    dut.io.dataRequest.valid #= false
    dut.io.dataRequest.engineId #= 1
    dut.io.dataRequest.vPifoId #= 0
    dut.io.pop.ready #= true
    dut.io.insert.foreach { in =>
      in.valid #= false
      in.engineId #= 1
      in.vPifoId #= 0
    }
    dut.io.controlRequest.valid #= false
    dut.io.controlRequest.command #= ControlCommand.UpdateMapperPost
    dut.io.controlRequest.engineId #= 1
    dut.io.controlRequest.vPifoId #= 1
    dut.io.controlRequest.flowId #= 0
    dut.io.controlRequest.data #= 0
    def reset(): Unit = {
      dut.io.controlRequest.valid #= false
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitFallingEdge()
    }
    def send(command: ControlCommand.E, key: Int, value: Int): Unit = {
      dut.io.controlRequest.valid #= true
      dut.io.controlRequest.command #= command
      dut.io.controlRequest.flowId #= key
      dut.io.controlRequest.data #= value
      dut.clockDomain.waitSamplingWhere(dut.io.controlRequest.ready.toBoolean)
      dut.clockDomain.waitFallingEdge()
      dut.io.controlRequest.valid #= false
    }
    import ControlCommand._
    for (command <- Seq(UpdateMapperPost, UpdateBrainFlowState)) {
      reset()
      send(command, 1, 1)
      send(command, 2, 2)
      send(CommitMapper, 0, 0)
      dut.clockDomain.waitSamplingWhere(dut.io.commitEpoch.toInt == 1)
      dut.clockDomain.waitSamplingWhere(!dut.io.replayBusy.toBoolean && dut.io.replayLogAvailable.toInt == 255)
      dut.clockDomain.waitFallingEdge()
      send(command, 3, 3) // no free slot: this command must remain pending
      send(CommitMapper, 0, 0)
      dut.clockDomain.waitSampling(60)
      assert(dut.io.camCapacityBlocked.toInt == 1)
      assert(dut.io.commitEpoch.toInt == 1, "commit overtook a full-table write")
    }
    println("CAM_MESH_CAPACITY_PASS post_and_flow_state=true blocked_commit=true")
    simSuccess()
  }
}
