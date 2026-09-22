package rio.sim

import scala.util.Random
import spinal.core.sim._
import rio._

/** Publish/replay equivalence and lookup visibility, using only the mapper IO. */
object ReplayMapperSim extends App {
  SimUtils.RioSimConfig.compile(ReplayMapper(3, 4)).doSim { dut =>
    SimTimeout(500000)
    dut.clockDomain.forkStimulus(period = 10)
    dut.io.readReq.valid #= false
    dut.io.readReq.payload #= 0
    dut.io.writeReq.valid #= false
    dut.io.writeReq.inputId #= 0
    dut.io.writeReq.outputId #= 0
    dut.io.commit #= false
    dut.clockDomain.assertReset()
    dut.clockDomain.waitSampling(4)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitSampling(2)

    def write(address: Int, value: Int): Unit = {
      dut.io.writeReq.valid #= true
      dut.io.writeReq.inputId #= address
      dut.io.writeReq.outputId #= value
      dut.clockDomain.waitSamplingWhere(dut.io.writeReq.ready.toBoolean)
      dut.io.writeReq.valid #= false
      dut.clockDomain.waitFallingEdge()
    }

    def read(address: Int, swap: Boolean = false): Int = {
      var result = -1
      val reader = fork {
        dut.clockDomain.waitSamplingWhere(dut.io.readRes.valid.toBoolean)
        result = dut.io.readRes.payload.toInt
      }
      dut.io.readReq.valid #= true
      dut.io.readReq.payload #= address
      dut.io.commit #= swap
      dut.clockDomain.waitSampling()
      dut.io.readReq.valid #= false
      dut.io.commit #= false
      reader.join()
      dut.clockDomain.waitFallingEdge()
      result
    }

    val random = new Random(0x52494f)
    var published = Array.fill(8)(0)
    var writes = 0
    for (epoch <- 0 until 96) {
      val updates = Vector.fill(epoch % 6)((random.nextInt(8), random.nextInt(16)))
      val next = published.clone()
      updates.foreach { case (address, value) =>
        write(address, value)
        next(address) = value
        writes += 1
        assert(read(address) == published(address), "staged write leaked into active bank")
      }
      val overlapAddress = epoch % 8
      assert(read(overlapAddress, swap = true) == published(overlapAddress), "swap-cycle read used new bank")
      published = next
      for (address <- 0 until 8) assert(read(address) == published(address))
      // The mesh supplies exactly this ordered replay through the same write port.
      updates.foreach { case (address, value) =>
        write(address, value)
        assert(read(address) == published(address), "replay disturbed active lookup")
      }
      // A further empty commit exposes the synchronized bank, including entries
      // untouched by this epoch and duplicate-address last-write semantics.
      assert(read(overlapAddress, swap = true) == published(overlapAddress))
      for (address <- 0 until 8) assert(read(address) == published(address))
    }
    println(s"REPLAY_MAPPER_PASS epochs=96 commits=192 writes=$writes overlap_reads=192")
    simSuccess()
  }
}
