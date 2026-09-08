package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import rio._
import SimUtils._

/**
 * Simple simulation for the Mapper component.
 * - Writes a mapping for each input -> (input % numOutputs)
 * - Reads each input back and checks the mapping
 *
 * Run with sbt from the `pifo-hardware` directory, for example:
 *   sbt "runMain rio.MapperSim"
 */
object MapperSim {
  def main(args: Array[String]): Unit = {
    // small mapper for test
    val numInputs = 16
    val numOutputs = 8

    // Use the same simulation harness style as PifoSim.scala: compile the DUT
    RioSimConfig
      .compile(new Mapper(numInputs, numOutputs))
      .doSim { dut =>

        // initialize inputs
        dut.io.readReq.valid #= false
        dut.io.writeReq.valid #= false

        // Reset + clock toggle thread (matched to PifoSim style)
        dut.clockDomain.assertReset()
        fork {
          while (true) {
            sleep(5)
            dut.clockDomain.clockToggle()
            sleep(5)
            dut.clockDomain.clockToggle()
          }
        }

        // short reset period
        dut.clockDomain.waitRisingEdge(10)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitRisingEdge(10)

        // helper to write a single mapping (use sampling semantics so translateFrom drives the
        // underlying Mem write port correctly)
        def writeMapping(in: Int, out: Int): Unit = {
          dut.io.writeReq.valid #= true
          dut.io.writeReq.payload.inputId #= in
          dut.io.writeReq.payload.outputId #= out
          // one sample to let translateFrom drive the write port
          dut.clockDomain.waitSampling()
          // deassert
          dut.io.writeReq.valid #= false
          // wait one more sample for stability
          dut.clockDomain.waitSampling()
        }

        // program the mapper: map input -> (input % numOutputs)
        for (i <- 0 until numInputs) {
          writeMapping(i, i % numOutputs)
        }

        // Small pause
        dut.clockDomain.waitRisingEdge(2)

        val readSeq = 0 until numInputs
        // read part
        val writer = fork {
          dut.clockDomain.waitRisingEdge(2)
          for (i <- readSeq) {
            dut.io.readReq.valid #= true
            dut.io.readReq.payload #= i

            // ram.flowReadSync provides a synchronous read; wait one sample for result
            dut.clockDomain.waitRisingEdge()
          }
          dut.io.readReq.valid #= false
        }

        // read response checking
        val reader = fork {
          for (i <- readSeq) {
            // wait for valid
            while (!dut.io.readRes.valid.toBoolean) {
              dut.clockDomain.waitRisingEdge()
            }
            val res = dut.io.readRes.payload.toInt
            val expected = i % numOutputs
            println(s"Mapper read: input=$i -> output=$res (expected=$expected)")
            assert(res == expected, s"Mapper read mismatch for input $i: got $res, expected $expected")

            // wait one clock before next
            dut.clockDomain.waitRisingEdge()
          }
        }

        writer.join()
        reader.join()

        println("All mappings verified successfully.")

        simSuccess()
      }
  }
}
