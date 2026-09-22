package rio.sim

import spinal.core.sim._
import rio._

/** Priority order, stable ties, port isolation, and empty responses. */
object BasicPifoSim extends App {
  val config = PifoConfig(numPifo = 8, bitPort = 2, bitPrio = 8, bitData = 8)
  SimConfig.withIVerilog.addSimulatorFlag("-g2012")
    .compile(new PifoRTL(config)).doSim { dut =>
    SimTimeout(10000)
    dut.io.push1.valid #= false
    dut.io.push2.valid #= false
    dut.io.popRequest.valid #= false
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(5)
    dut.clockDomain.waitFallingEdge()

    def push(port: Int, priority: Int, data: Int, second: Boolean = false): Unit = {
      val input = if (second) dut.io.push2 else dut.io.push1
      input.valid #= true
      input.port #= port
      input.priority #= priority
      input.data #= data
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitFallingEdge()
      input.valid #= false
    }

    def pop(port: Int, expected: Option[(Int, Int)]): Unit = {
      dut.io.popRequest.valid #= true
      dut.io.popRequest.port #= port
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitFallingEdge()
      dut.io.popRequest.valid #= false
      assert(dut.io.popResponse.valid.toBoolean)
      assert(dut.io.popResponse.port.toInt == port)
      assert(dut.io.popResponse.exist.toBoolean == expected.nonEmpty, s"port=$port expected=$expected")
      expected.foreach { entry =>
        val actual = (dut.io.popResponse.priority.toInt, dut.io.popResponse.data.toInt)
        assert(actual == entry, s"port=$port expected=$entry got=$actual")
      }
    }

    pop(1, None)
    push(1, 20, 1)
    push(2, 1, 2, second = true)
    push(1, 5, 3)
    push(1, 5, 4, second = true)
    pop(1, Some((5, 3)))
    pop(1, Some((5, 4)))
    pop(1, Some((20, 1)))
    pop(1, None)
    pop(2, Some((1, 2)))
    pop(2, None)
    println("[BasicPifoSim] priority order, stable ties, port isolation and empty responses passed")
  }
}
