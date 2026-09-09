package rio.sim

import spinal.core.sim._

import rio._

/** Cycle-level checks for transactional mapper visibility and bank resynchronization.
  *
  * Run from pifo-hardware with:
  *   sbt "runMain rio.sim.TransactionalMapperSim"
  */
object TransactionalMapperSim extends App {
  private val inputWidth = 3
  private val outputWidth = 4

  SimUtils.RioSimConfig
    .compile(TransactionalMapper(inputWidth, outputWidth))
    .doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      dut.io.readReq.valid #= false
      dut.io.readReq.payload #= 0
      dut.io.writeReq.valid #= false
      dut.io.writeReq.payload.inputId #= 0
      dut.io.writeReq.payload.outputId #= 0
      dut.io.commit #= false

      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(2)

      def writeMapping(input: Int, output: Int): Unit = {
        dut.io.writeReq.valid #= true
        dut.io.writeReq.payload.inputId #= input
        dut.io.writeReq.payload.outputId #= output
        dut.clockDomain.waitSamplingWhere(dut.io.writeReq.ready.toBoolean)
        dut.io.writeReq.valid #= false
      }

      def readMapping(input: Int): Int = {
        var result = -1
        val reader = fork {
          dut.clockDomain.waitSamplingWhere(dut.io.readRes.valid.toBoolean)
          result = dut.io.readRes.payload.toInt
        }
        dut.io.readReq.valid #= true
        dut.io.readReq.payload #= input
        dut.clockDomain.waitSampling()
        dut.io.readReq.valid #= false
        reader.join()
        result
      }

      def commit(): Unit = {
        assert(dut.io.commitReady.toBoolean, "commit issued while mapper was synchronizing")
        dut.io.commit #= true
        dut.clockDomain.waitSampling()
        dut.io.commit #= false
        dut.clockDomain.waitSamplingWhere(!dut.io.commitReady.toBoolean)
      }

      def waitUntilReady(): Unit = {
        while (!dut.io.commitReady.toBoolean) {
          dut.clockDomain.waitSampling()
        }
      }

      assert(readMapping(1) == 0)
      writeMapping(1, 3)
      writeMapping(2, 5)

      // Backup-bank writes do not affect packets before commit.
      assert(readMapping(1) == 0)
      assert(readMapping(2) == 0)

      // A read accepted on the commit edge belongs to the old transaction.
      dut.io.readReq.valid #= true
      dut.io.readReq.payload #= 1
      dut.io.commit #= true
      var commitEdgeRead = -1
      val commitEdgeReader = fork {
        dut.clockDomain.waitSamplingWhere(dut.io.readRes.valid.toBoolean)
        commitEdgeRead = dut.io.readRes.payload.toInt
      }
      dut.clockDomain.waitSampling()
      dut.io.readReq.valid #= false
      dut.io.commit #= false
      commitEdgeReader.join()
      assert(commitEdgeRead == 0)
      assert(!dut.io.commitReady.toBoolean)

      // Reads after that edge all use the newly active bank, even during sync.
      assert(readMapping(1) == 3)
      assert(readMapping(2) == 5)
      assert(!dut.io.writeReq.ready.toBoolean, "updates must wait for backup-bank synchronization")
      waitUntilReady()

      // The sync preserves untouched entries in the next transaction's bank.
      writeMapping(2, 7)
      assert(readMapping(1) == 3)
      assert(readMapping(2) == 5)
      commit()
      assert(readMapping(1) == 3)
      assert(readMapping(2) == 7)
      waitUntilReady()

      simSuccess()
    }
}
