package rio.sim

import rio._
import scala.collection.mutable
import scala.util.Random
import spinal.core.sim._

/** Reference-map checks include sparse 23-bit keys, a non-power-of-two capacity,
  * full-bank updates/deletes, read/write collisions, reset and replay epochs.
  */
object BoundedCamMapperSim extends App {
  for (capacity <- Seq(2, 3, 16); replay <- Seq(false, true)) {
    SimUtils.RioSimConfig.compile(BoundedCamMapper(23, 13, capacity, replay)).doSim { dut =>
      SimTimeout(2000000)
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.readReq.valid #= false
      dut.io.readReq.payload #= 0
      dut.io.writeReq.valid #= false
      dut.io.writeReq.inputId #= 0
      dut.io.writeReq.outputId #= 0
      dut.io.commit #= false
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitFallingEdge()

      val contents = Array.fill(if (replay) 2 else 1)(mutable.Map.empty[Int, Int])
      var active = 0
      var cycles = 0
      var rejected = 0
      val keys = Vector.tabulate(capacity + 5)(i => if (i == 0) 0 else ((i * 262147) ^ 0x400001) & 0x7fffff)
      val random = new Random(0x43414d + capacity)

      def tick(read: Option[Int] = None, write: Option[(Int, Int)] = None, commit: Boolean = false): Boolean = {
        val expected = read.map(key => contents(active).getOrElse(key, 0))
        val target = if (replay) 1 - active else 0
        val canWrite = write.forall { case (key, value) =>
          value == 0 || contents(target).contains(key) || contents(target).size < capacity
        }
        dut.io.readReq.valid #= read.nonEmpty
        dut.io.readReq.payload #= read.getOrElse(0).toLong
        dut.io.writeReq.valid #= write.nonEmpty
        dut.io.writeReq.inputId #= write.map(_._1).getOrElse(0).toLong
        dut.io.writeReq.outputId #= write.map(_._2).getOrElse(0).toLong
        dut.io.commit #= commit
        sleep(1)
        if (write.nonEmpty) assert(dut.io.writeReq.ready.toBoolean == canWrite)
        assert(dut.io.capacityBlocked.toBoolean == (write.nonEmpty && !canWrite))
        dut.clockDomain.waitSampling()
        sleep(1)
        assert(dut.io.readRes.valid.toBoolean == read.nonEmpty)
        expected.foreach { value =>
          assert(dut.io.readRes.payload.toInt == value,
            s"capacity=$capacity replay=$replay cycle=$cycles key=$read expected=$value got=${dut.io.readRes.payload.toInt}")
        }
        if (canWrite) write.foreach { case (key, value) =>
          if (value == 0) contents(target).remove(key) else contents(target)(key) = value
        } else rejected += 1
        if (replay && commit) active = 1 - active
        cycles += 1
        dut.clockDomain.waitFallingEdge()
        canWrite
      }

      def epoch(writes: Seq[(Int, Int)]): Unit = {
        val accepted = writes.filter { case (key, value) =>
          tick(Some(key), Some((key, value)))
        }
        if (replay) {
          tick(Some(keys.head), commit = true)
          accepted.foreach { w => assert(tick(Some(w._1), Some(w))) }
          assert(contents(0) == contents(1), "replay did not restore equivalent bank contents")
          tick(Some(keys.last), commit = true) // empty epoch exposes the restored bank
        } else {
          tick(Some(keys.head), commit = true) // ordinary CAM ignores commits
        }
        keys.foreach(key => tick(Some(key)))
      }

      keys.foreach(key => tick(Some(key))) // reset/miss, including key zero
      epoch(keys.take(capacity).zipWithIndex.map { case (key, i) => key -> (i + 1) })
      epoch(Seq(keys.last -> 77, keys.head -> 8191, keys.head -> 0, keys.last -> 55))
      for (_ <- 0 until 80) {
        epoch(Vector.fill(12) {
          val key = keys(random.nextInt(keys.size))
          val value = if (random.nextInt(4) == 0) 0 else 1 + random.nextInt(8191)
          key -> value
        })
      }
      dut.io.readReq.valid #= false
      dut.io.writeReq.valid #= false
      dut.io.commit #= false
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitFallingEdge()
      contents.foreach(_.clear())
      active = 0
      keys.foreach(key => tick(Some(key)))
      assert(rejected > 0)
      println(s"BOUNDED_CAM_PASS capacity=$capacity replay=$replay cycles=$cycles rejected_full_writes=$rejected")
      simSuccess()
    }
  }
}
