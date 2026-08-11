package rio.sim

import spinal.core._
import spinal.core.sim._

import rio._

/** Integration checks for mesh-wide transactional configuration.
  *
  * Run from pifo-hardware with:
  *   sbt "runMain rio.sim.TransactionalConfigSim"
  */
object TransactionalConfigSim extends App {
  private val config = EngineConfig(
    numEngines = 2,
    numVPIFOs = 4,
    maxPacketPriority = 32,
    fifoDepth = 2,
    prefetchBufferDepth = 2
  )

  SimConfig.withIVerilog
    .addSimulatorFlag("-g2012")
    .compile {
      val mesh = PifoMesh(config)
      mesh.pifoEngines.foreach { engine =>
        engine.enque.brain.io.request.valid.simPublic()
        engine.enque.brain.io.request.ready.simPublic()
        engine.enque.brain.io.request.payload.vpifoId.simPublic()
        engine.enque.brain.io.response.valid.simPublic()
        engine.enque.brain.io.response.payload.priority.simPublic()
        engine.enque.enqueMapper.activeBank.simPublic()
        engine.deque.dequeMapper.activeBank.simPublic()
        engine.deque.nonExistMapper.activeBank.simPublic()
      }
      mesh
    }
    .doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      dut.io.dataRequest.valid #= false
      dut.io.dataRequest.payload.engineId #= 0
      dut.io.dataRequest.payload.vPifoId #= 0
      dut.io.pop.ready #= true
      dut.io.controlRequest.valid #= false
      dut.io.controlRequest.payload.command #= ControlCommand.UpdateMapperPre
      dut.io.controlRequest.payload.engineId #= 0
      dut.io.controlRequest.payload.vPifoId #= 0
      dut.io.controlRequest.payload.flowId #= 0
      dut.io.controlRequest.payload.data #= 0
      dut.io.insert.foreach { insert =>
        insert.valid #= false
        insert.payload.engineId #= 0
        insert.payload.vPifoId #= 0
      }

      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(4)

      def sendControl(
          command: ControlCommand.E,
          engineId: Int,
          vPifoId: Int = 0,
          flowId: Int = 0,
          data: Int = 0
      ): Unit = {
        dut.io.controlRequest.valid #= true
        dut.io.controlRequest.payload.command #= command
        dut.io.controlRequest.payload.engineId #= engineId
        dut.io.controlRequest.payload.vPifoId #= vPifoId
        dut.io.controlRequest.payload.flowId #= flowId
        dut.io.controlRequest.payload.data #= data
        dut.clockDomain.waitSamplingWhere(dut.io.controlRequest.ready.toBoolean)
        dut.io.controlRequest.valid #= false
        dut.clockDomain.waitSampling()
      }

      def enqueueAndObserveMappings(flowId: Int): Seq[Int] = {
        val observed = Array.fill(config.numEngines)(-1)
        val observers = dut.pifoEngines.zipWithIndex.map { case (engine, index) =>
          fork {
            dut.clockDomain.waitSamplingWhere(
              engine.enque.brain.io.request.valid.toBoolean &&
                engine.enque.brain.io.request.ready.toBoolean
            )
            observed(index) = engine.enque.brain.io.request.payload.vpifoId.toInt
          }
        }
        val drivers = dut.io.insert.zipWithIndex.map { case (insert, index) =>
          fork {
            insert.valid #= true
            insert.payload.engineId #= index + 1
            insert.payload.vPifoId #= flowId
            dut.clockDomain.waitSamplingWhere(insert.ready.toBoolean)
            insert.valid #= false
          }
        }
        drivers.foreach(_.join())
        observers.foreach(_.join())
        observed.toSeq
      }

      // These updates only touch backup banks.
      sendControl(ControlCommand.UpdateMapperPre, engineId = 1, vPifoId = 1, data = 2)
      sendControl(ControlCommand.UpdateMapperPre, engineId = 2, vPifoId = 1, data = 3)

      // Brain configuration remains immediate and does not need a mapper commit.
      sendControl(ControlCommand.UpdateBrainEngine, engineId = 1, vPifoId = 0, data = 3)
      dut.clockDomain.waitSampling(4)

      var immediateBrainPriority = -1
      val brainResponse = fork {
        dut.clockDomain.waitSamplingWhere(dut.pifoEngines.head.enque.brain.io.response.valid.toBoolean)
        immediateBrainPriority = dut.pifoEngines.head.enque.brain.io.response.payload.priority.toInt
      }
      assert(enqueueAndObserveMappings(flowId = 1) == Seq(0, 0))
      brainResponse.join()
      assert(immediateBrainPriority == 1, "brain update was incorrectly delayed until mapper commit")

      dut.clockDomain.waitSampling(4)

      // Observe every mapper bank to verify the global commit edge is shared.
      val activeBanks = dut.pifoEngines.flatMap { engine =>
        Seq(
          engine.enque.enqueMapper.activeBank,
          engine.deque.dequeMapper.activeBank,
          engine.deque.nonExistMapper.activeBank
        )
      }
      assert(activeBanks.forall(bank => !bank.toBoolean))
      val switchTimes = Array.fill(activeBanks.size)(-1L)
      val switchObservers = activeBanks.zipWithIndex.map { case (bank, index) =>
        fork {
          dut.clockDomain.waitSamplingWhere(bank.toBoolean)
          switchTimes(index) = simTime()
        }
      }

      sendControl(ControlCommand.CommitMapper, engineId = 1)
      switchObservers.foreach(_.join())
      assert(
        switchTimes.distinct.length == 1,
        s"mapper banks switched on different cycles: ${switchTimes.mkString(",")}"
      )
      assert(!dut.io.commitReady.toBoolean)

      // Both engines expose their new mappings immediately after that edge while
      // the old banks are still being synchronized in the background.
      assert(enqueueAndObserveMappings(flowId = 1) == Seq(2, 3))

      while (!dut.io.commitReady.toBoolean) {
        dut.clockDomain.waitSampling()
      }

      simSuccess()
    }
}
