package rio.sim

import rio._
import spinal.core.sim._
import scala.collection.mutable
import scala.util.Random

/** Scoreboard uses contents at request acceptance, not the implementation's
  * pipeline state. Checks ordering/latency, update completion and replay under
  * output stalls, partial stages, full banks and reset during an SRL update.
  */
object PipelinedCamMapperSim extends App {
  val cases = if (args.headOption.contains("small")) Seq((3, 2), (33, 4))
    else Seq((3, 2), (33, 4), (129, 128), (257, 128), (513, 256))
  for ((capacity, perStage) <- cases; replay <- Seq(false, true)) {
    SimConfig.withIVerilog.addSimulatorFlag("-g2012")
      .compile(PipelinedCamMapper(23, 13, capacity, replay, perStage))
      .doSim(seed = 0x43414d + capacity) { dut =>
        SimTimeout(20000000)
        dut.clockDomain.forkStimulus(10)
        dut.io.readReq.valid #= false
        dut.io.readReq.payload #= 0
        dut.io.readRes.ready #= false
        dut.io.writeReq.valid #= false
        dut.io.writeReq.inputId #= 0
        dut.io.writeReq.outputId #= 0
        dut.io.commit #= false

        val random = new Random(0x43414d + capacity)
        val model = Array.fill(if (replay) 2 else 1)(mutable.Map.empty[Int, Int])
        val expected = mutable.Queue.empty[(Int, Int)] // value and admission cycle
        val keys = Vector.tabulate(capacity + 16)(i => (i * 262147) & 0x7fffff)
        var active = 0
        var cycle = 0
        var pendingRead = Option.empty[Int]
        var heldOutput = Option.empty[Int]
        var reads = 0
        var writes = 0
        var rejectedCommits = 0
        var readStalls = 0
        var outputStalls = 0
        var maxWriteCycles = 0
        var exactLatency = false

        // Inputs are driven at falling edges; the sampled output precedes the
        // next rising edge. This makes the advertised N+1-cycle latency exact.
        def step(nextRead: Option[Int] = None, write: Option[(Int, Int)] = None,
                 commit: Boolean = false, ready: Boolean = true): (Boolean, Boolean, Boolean) = {
          if (pendingRead.isEmpty) pendingRead = nextRead
          dut.io.readReq.valid #= pendingRead.nonEmpty
          dut.io.readReq.payload #= pendingRead.getOrElse(0).toLong
          dut.io.readRes.ready #= ready
          dut.io.writeReq.valid #= write.nonEmpty
          dut.io.writeReq.inputId #= write.map(_._1).getOrElse(0).toLong
          dut.io.writeReq.outputId #= write.map(_._2).getOrElse(0).toLong
          dut.io.commit #= commit
          sleep(1)
          val readFire = pendingRead.nonEmpty && dut.io.readReq.ready.toBoolean
          val writeFire = write.nonEmpty && dut.io.writeReq.ready.toBoolean
          val commitFire = commit && dut.io.commitReady.toBoolean
          heldOutput.foreach { value =>
            assert(dut.io.readRes.valid.toBoolean && dut.io.readRes.payload.toInt == value,
              "response changed under backpressure")
          }
          heldOutput = None
          if (dut.io.readRes.valid.toBoolean) {
            assert(expected.nonEmpty, "unsolicited/duplicate response")
            val (value, accepted) = expected.front
            assert(dut.io.readRes.payload.toInt == value,
              s"capacity=$capacity stage=$perStage replay=$replay cycle=$cycle expected=$value got=${dut.io.readRes.payload.toInt}")
            assert(cycle - accepted >= dut.minimumReadLatency)
            if (exactLatency) assert(cycle - accepted == dut.minimumReadLatency)
            if (ready) expected.dequeue() else {
              heldOutput = Some(value)
              outputStalls += 1
            }
          }
          if (readFire) {
            expected.enqueue(model(active).getOrElse(pendingRead.get, 0) -> cycle)
            pendingRead = None
            reads += 1
          } else if (pendingRead.nonEmpty) readStalls += 1
          if (writeFire) {
            val (key, value) = write.get
            val target = if (replay) 1 - active else 0
            assert(value == 0 || model(target).contains(key) || model(target).size < capacity,
              "full insertion was acknowledged")
            if (value == 0) model(target).remove(key) else model(target)(key) = value
            writes += 1
          }
          if (commit && !commitFire) rejectedCommits += 1
          if (commitFire && replay) active = 1 - active
          dut.clockDomain.waitSampling()
          sleep(1)
          cycle += 1
          dut.clockDomain.waitFallingEdge()
          (readFire, writeFire, commitFire)
        }

        def reset(): Unit = {
          dut.io.readReq.valid #= false
          dut.io.writeReq.valid #= false
          dut.io.commit #= false
          dut.clockDomain.assertReset()
          dut.clockDomain.waitRisingEdge(4)
          dut.clockDomain.deassertReset()
          dut.clockDomain.waitFallingEdge()
          model.foreach(_.clear())
          expected.clear()
          pendingRead = None
          heldOutput = None
          active = 0
          var waited = 0
          while (!dut.io.initialized.toBoolean) {
            step()
            waited += 1
            assert(waited < 100)
          }
          step()
        }

        def randomRead: Option[Int] = Some(keys(random.nextInt(keys.size)))
        def update(key: Int, value: Int, ready: Boolean = true): Unit = {
          var elapsed = 0
          var done = false
          while (!done) {
            // A commit presented while busy must not publish partial contents.
            val result = step(randomRead, Some(key -> value), commit = elapsed > 0, ready = ready)
            assert(!result._3, "commit passed an unfinished update")
            done = result._2
            elapsed += 1
            assert(elapsed < 200 + 3 * dut.stageCount, "write did not complete")
          }
          maxWriteCycles = maxWriteCycles.max(elapsed)
        }

        def commit(ready: Boolean = true): Unit = {
          var elapsed = 0
          var done = false
          while (!done) {
            done = step(randomRead, commit = true, ready = ready)._3
            elapsed += 1
            assert(elapsed < 200)
          }
        }

        def drain(): Unit = {
          var elapsed = 0
          while (pendingRead.nonEmpty || expected.nonEmpty) {
            step()
            elapsed += 1
            assert(elapsed < 200 + 3 * dut.minimumReadLatency)
          }
          step()
        }

        def epoch(commands: Seq[(Int, Int)], ready: Boolean = true): Unit = {
          commands.foreach { case (key, value) => update(key, value, ready) }
          commit(ready)
          if (replay) {
            commands.foreach { case (key, value) => update(key, value, ready) }
            assert(model(0) == model(1), "replay did not restore both banks")
          }
        }

        def burst(requests: Seq[Int]): Unit = {
          drain()
          exactLatency = true
          val start = cycle
          requests.foreach(key => assert(step(Some(key))._1, "unstalled lookup lost II=1"))
          assert(cycle - start == requests.size)
          while (expected.nonEmpty) step()
          exactLatency = false
        }

        reset()
        burst(keys) // misses, including key zero and high sparse keys
        epoch(keys.take(capacity).zipWithIndex.map { case (key, i) => key -> (1 + i % 8191) })
        burst(keys ++ keys.reverse)
        // A full bank still permits value updates and deletion, then slot reuse.
        epoch(Seq(keys.head -> 8191, keys.last -> 0, keys(capacity - 1) -> 0, keys.last -> 55))
        burst(keys)
        // Bank publication, replay, and an empty epoch while responses are held.
        for (_ <- 0 until 4 * dut.minimumReadLatency + 20) step(randomRead, ready = false)
        epoch(Seq(keys.head -> 77), ready = false)
        commit(ready = false)
        drain()
        burst(keys)

        // Sparse, repeated-key updates and deletes over many bank swaps.
        for (_ <- 0 until 20) {
          val target = if (replay) 1 - active else 0
          val scratch = mutable.Map.from(model(target))
          val commands = Vector.fill(6) {
            val key = keys(random.nextInt(keys.size))
            val value = if (random.nextInt(3) == 0 ||
              (!scratch.contains(key) && scratch.size == capacity)) 0 else 1 + random.nextInt(8191)
            if (value == 0) scratch.remove(key) else scratch(key) = value
            key -> value
          }
          epoch(commands)
        }
        drain()
        // Deterministically refill both banks before checking a blocked insert.
        val current = model(if (replay) 1 - active else 0).keySet.toSet
        val additions = keys.filterNot(current).take(capacity - current.size).map(_ -> 31)
        epoch(additions)
        val absent = keys.find(k => !model(if (replay) 1 - active else 0).contains(k)).get
        for (i <- 0 until 100 + 2 * dut.stageCount) {
          val result = step(randomRead, Some(absent -> 81), commit = i > 0)
          assert(!result._2 && !result._3, "a full insert or following commit passed")
        }
        assert(dut.io.capacityBlocked.toBoolean)

        // Reset cancels the blocked stream, clears every CAM stage and discards
        // old responses. Then interrupt a real SRL insertion before it finishes.
        reset()
        burst(keys)
        val command = Some(keys.last -> 93)
        for (_ <- 0 until dut.stageCount + 6) {
          assert(!step(randomRead, command, ready = false)._2)
        }
        reset()
        burst(keys)
        assert(readStalls > 0 && outputStalls > 0 && rejectedCommits > 0)
        println(s"PIPELINED_CAM_PASS capacity=$capacity entries_per_stage=$perStage replay=$replay " +
          s"latency=${dut.minimumReadLatency} reads=$reads writes=$writes read_stalls=$readStalls " +
          s"output_stalls=$outputStalls rejected_commits=$rejectedCommits max_write_cycles=$maxWriteCycles")
        simSuccess()
      }
  }
}
