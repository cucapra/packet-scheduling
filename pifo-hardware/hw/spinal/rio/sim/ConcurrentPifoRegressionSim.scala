package rio.sim

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import spinal.core.sim._
import rio._

object ConcurrentPifoRegressionSim extends App {
  val config = PifoConfig(512, 4, 24, 7)
  SimConfig.withVerilator.compile {
    val pifo = new ConcurrentPifoRTL(config)
    pifo
  }.doSim { dut =>
    val random = new Random(712)
    val entries = ArrayBuffer.empty[(Int, Int, Int)]
    dut.clockDomain.forkStimulus(10)
    dut.io.push1.valid #= false
    dut.io.push2.valid #= false
    dut.io.popRequest.valid #= false
    dut.clockDomain.waitSampling(5)
    dut.clockDomain.waitFallingEdge()
    for (cycle <- 0 until 2000) {
      val port = random.nextInt(4) + 1
      val pop = entries.nonEmpty && random.nextDouble() < .6
      val position = entries.indexWhere(_._1 == port)
      val expected = if (pop && position >= 0) Some(entries.remove(position)) else None
      dut.io.popRequest.valid #= pop
      dut.io.popRequest.port #= port
      for (push <- Seq(dut.io.push1, dut.io.push2)) {
        val valid = entries.size < config.numPifo && random.nextDouble() < .35
        val entry = (random.nextInt(4) + 1, random.nextInt(10000) + 1, random.nextInt(128))
        push.valid #= valid
        push.port #= entry._1
        push.priority #= entry._2
        push.data #= entry._3
        if (valid) {
          val at = entries.indexWhere(_._2 > entry._2)
          entries.insert(if (at < 0) entries.size else at, entry)
        }
      }
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitFallingEdge()
      assert(dut.io.popResponse.valid.toBoolean == pop, s"cycle=$cycle valid")
      if (pop) {
        assert(dut.io.popResponse.exist.toBoolean == expected.nonEmpty,
          s"cycle=$cycle expected=$expected exists=${dut.io.popResponse.exist.toBoolean} data=${dut.io.popResponse.data.toInt} rank=${dut.io.popResponse.priority.toInt}")
        expected.foreach { e =>
          assert((dut.io.popResponse.port.toInt, dut.io.popResponse.priority.toInt, dut.io.popResponse.data.toInt) == e,
            s"cycle=$cycle expected=$e got=${dut.io.popResponse.port.toInt},${dut.io.popResponse.priority.toInt},${dut.io.popResponse.data.toInt}")
        }
      }
    }
    println("[ConcurrentPifoRegressionSim] 2000 simultaneous-operation cycles passed")
  }
}
