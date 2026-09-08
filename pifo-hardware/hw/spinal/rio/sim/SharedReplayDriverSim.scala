package rio.sim

import rio._
import spinal.core._
import spinal.core.sim._

/** Driver regression: empty commits must finish and oversized epochs must fail
  * before presenting the command that would prevent the reserved commit.
  */
object SharedReplayDriverSim extends App {
  val config = EngineConfig(2, 8, 256, 4, 2, commitQueueLength = 4)
  def rejected(body: => Unit): Unit = {
    val failure = scala.util.Try(body).failed.get
    assert(failure.isInstanceOf[IllegalArgumentException])
    assert(failure.getMessage.contains("--control-queue-depth"))
  }
  import ControlCommand._
  val epoch = Seq(UpdateMapperPre, UpdateBrainState, UpdateMapperPost, CommitMapper)
  ReplayEpochCapacity.validate(config, Seq.fill(100)(UpdateBrainState) ++ epoch ++ epoch)
  rejected(ReplayEpochCapacity.validate(config, epoch.dropRight(1) ++ Seq(UpdateMapperNonExist, CommitMapper)))
  ReplayEpochCapacity.validate(config.copy(commitQueueLength = 8), epoch.dropRight(1) ++ epoch)
  ReplayEpochCapacity.validate(config.copy(dynamicConfig = false), Seq.fill(100)(UpdateMapperPre))

  SimConfig.withIVerilog.addSimulatorFlag("-g2012").compile {
    val mesh = new PifoMesh(config)
    mesh.pifoEngines.foreach(_.enque.enqueMapper.activeBank.simPublic())
    mesh
  }.doSim { dut =>
    SimTimeout(10000)
    dut.io.controlRequest.valid #= false
    dut.io.dataRequest.valid #= false
    dut.io.dataRequest.payload.engineId #= 1
    dut.io.dataRequest.payload.vPifoId #= 0
    dut.io.pop.ready #= true
    dut.io.insert.foreach { port =>
      port.valid #= false
      port.payload.engineId #= 1
      port.payload.vPifoId #= 0
    }
    val controller = PifoMeshSimController(config, dut)
    controller.start(enableControlSocket = false, SimUtils.DefaultControlSocketPath, monitorPops = false)
    var callbacks = 0
    var expectedBank = false
    def commit(): Unit = {
      expectedBank = !expectedBank
      controller.sendControl(CommitMapper, 1, 0, onCommitApplied = () => {
        callbacks += 1
        assert(dut.pifoEngines.forall(_.enque.enqueMapper.activeBank.toBoolean == expectedBank))
      })
      assert(!dut.io.replayBusy.toBoolean)
      assert(dut.io.replayLogAvailable.toInt == config.commitQueueLength - 1)
    }
    commit()
    commit()
    for (k <- 0 until 12) controller.sendControl(UpdateBrainEngine, 1, 3, vPifoId = k % 8)
    commit()
    for (epoch <- 0 until 8) {
      controller.sendControl(UpdateMapperPre, 1, epoch % 8, vPifoId = epoch % 8)
      controller.sendControl(UpdateBrainState, 1, epoch, vPifoId = epoch % 8)
      controller.sendControl(UpdateMapperPost, 2, epoch, vPifoId = epoch % 8, flowId = epoch)
      rejected(controller.sendControl(UpdateMapperNonExist, 1, 0))
      assert(!dut.io.controlRequest.valid.toBoolean)
      commit()
    }
    rejected(RequestSimulationConfiguration.configureFlatFifo(config, controller, 1, 0, Seq(0, 1)))
    assert(callbacks == 11)
    println(s"SHARED_REPLAY_DRIVER_PASS commits=$callbacks empty_commits=3 rejected_epochs=9")
    simSuccess()
  }
}
