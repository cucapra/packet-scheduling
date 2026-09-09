package rio.sim

import rio._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable

/** Exercise the real shared replay FIFO, including commits already queued
  * behind an epoch. No second command FIFO/journal is added by the mapper.
  */
case class PipelinedCamReplayHarness() extends Component {
  val config = EngineConfig(1, 4, 256, 2, 2, commitQueueLength = 256)
  val io = new Bundle {
    val control = slave Stream(ControlMessage(config))
    val readReq = slave Stream(UInt(5 bits))
    val readRes = master Stream(UInt(13 bits))
    val applied = master Flow(MapperUpdater(5, 13))
    val commitApplied, replaying, initialized = out Bool()
  }
  val fifo = ReplayControlFifo(config)
  val mapper = PipelinedCamMapper(5, 13, 16, replay = true, entriesPerStage = 2)
  fifo.io.push << io.control
  mapper.io.readReq << io.readReq
  io.readRes << mapper.io.readRes
  val isCommit = fifo.io.pop.command === ControlCommand.CommitMapper
  mapper.io.writeReq.valid := fifo.io.pop.valid && !isCommit
  mapper.io.writeReq.inputId := fifo.io.pop.vPifoId @@ fifo.io.pop.flowId
  mapper.io.writeReq.outputId := fifo.io.pop.data.resized
  mapper.io.commit := fifo.io.pop.valid && isCommit && mapper.io.commitReady
  fifo.io.pop.ready := Mux(isCommit, mapper.io.commitReady, mapper.io.writeReq.ready)
  io.commitApplied := mapper.io.commit
  io.applied.valid := mapper.io.writeReq.fire
  io.applied.payload := mapper.io.writeReq.payload
  io.replaying := fifo.io.replaying
  io.initialized := mapper.io.initialized
}

object PipelinedCamReplayIntegrationSim extends App {
  SimConfig.withIVerilog.addSimulatorFlag("-g2012")
    .compile(PipelinedCamReplayHarness()).doSim(seed = 0x52504c) { dut =>
      SimTimeout(2000000)
      dut.clockDomain.forkStimulus(10)
      dut.io.control.valid #= false
      dut.io.control.command #= ControlCommand.UpdateMapperPost
      dut.io.control.engineId #= 1
      dut.io.control.vPifoId #= 0
      dut.io.control.flowId #= 0
      dut.io.control.data #= 0
      dut.io.readReq.valid #= false
      dut.io.readReq.payload #= 0
      dut.io.readRes.ready #= false
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitFallingEdge()

      val epochs = Seq(
        Seq(0 -> 101, 3 -> 103, 7 -> 107, 16 -> 116, 31 -> 131),
        Seq(0 -> 201, 7 -> 0, 8 -> 208, 0 -> 301),
        Seq.empty[(Int, Int)],
        Seq(3 -> 303, 31 -> 0, 30 -> 330),
        Seq.empty[(Int, Int)],
        Seq(16 -> 416, 7 -> 407))
      // None denotes commit. The expected event stream includes both passes
      // through the *same* FIFO; a commit cannot overtake its replay writes.
      val commands = mutable.Queue.from(epochs.flatMap(e => e.map(Some(_)) :+ None))
      val events = mutable.Queue.from(epochs.flatMap { e =>
        e.map(w => (Some(w), false)) ++ Seq((None, false)) ++ e.map(w => (Some(w), true))
      })
      val model = Array.fill(2)(mutable.Map.empty[Int, Int])
      val responses = mutable.Queue.empty[Int]
      var active = 0
      var pendingRead = Option.empty[Int]
      var heldResponse = Option.empty[Int]
      var cycle = 0
      var commits = 0
      var applied = 0
      var readCount = 0
      var readStalls = 0
      var replayCycles = 0
      var quiet = 0

      while (quiet < 40) {
        assert(cycle < 10000)
        val running = commands.nonEmpty || events.nonEmpty
        if (pendingRead.isEmpty && running) pendingRead = Some((cycle * 7) & 31)
        val consumerReady = cycle % 211 >= 55
        dut.io.readReq.valid #= pendingRead.nonEmpty
        dut.io.readReq.payload #= pendingRead.getOrElse(0).toLong
        dut.io.readRes.ready #= consumerReady
        dut.io.control.valid #= commands.nonEmpty
        commands.headOption.foreach {
          case Some((key, value)) =>
            dut.io.control.command #= ControlCommand.UpdateMapperPost
            dut.io.control.vPifoId #= key >> 3
            dut.io.control.flowId #= key & 7
            dut.io.control.data #= value
          case None =>
            dut.io.control.command #= ControlCommand.CommitMapper
            dut.io.control.vPifoId #= 0
            dut.io.control.flowId #= 0
            dut.io.control.data #= 0
        }
        sleep(1)
        heldResponse.foreach(value => assert(dut.io.readRes.valid.toBoolean &&
          dut.io.readRes.payload.toInt == value, "stalled response changed"))
        heldResponse = None
        if (dut.io.readRes.valid.toBoolean) {
          assert(responses.nonEmpty && dut.io.readRes.payload.toInt == responses.front,
            s"lookup snapshot/order mismatch at cycle $cycle")
          if (consumerReady) responses.dequeue() else heldResponse = Some(responses.front)
        }
        if (pendingRead.nonEmpty && dut.io.readReq.ready.toBoolean) {
          responses.enqueue(model(active).getOrElse(pendingRead.get, 0))
          pendingRead = None
          readCount += 1
        } else if (pendingRead.nonEmpty) readStalls += 1
        if (dut.io.applied.valid.toBoolean) {
          val write = dut.io.applied.inputId.toInt -> dut.io.applied.outputId.toInt
          val event = (Some(write), dut.io.replaying.toBoolean)
          assert(events.nonEmpty && events.dequeue() == event, s"unexpected write/replay: $event")
          if (write._2 == 0) model(1 - active).remove(write._1) else model(1 - active)(write._1) = write._2
          applied += 1
        }
        if (dut.io.commitApplied.toBoolean) {
          assert(!dut.io.replaying.toBoolean, "commit passed unfinished FIFO replay")
          assert(events.nonEmpty && events.dequeue() == ((None, false)), "early/duplicate commit")
          active = 1 - active
          commits += 1
        }
        if (dut.io.replaying.toBoolean) replayCycles += 1
        if (commands.nonEmpty && dut.io.control.ready.toBoolean) commands.dequeue()
        if (!running && pendingRead.isEmpty && responses.isEmpty) quiet += 1 else quiet = 0
        dut.clockDomain.waitSampling()
        sleep(1)
        dut.clockDomain.waitFallingEdge()
        cycle += 1
      }
      assert(model(0) == model(1))
      assert(commits == epochs.size && applied == 2 * epochs.map(_.size).sum)
      assert(readStalls > 0 && replayCycles > 0 && readCount > 0)
      println(s"PIPELINED_CAM_REPLAY_PASS fifo_depth=256 epochs=$commits writes=$applied " +
        s"reads=$readCount read_stalls=$readStalls replay_cycles=$replayCycles cycles=$cycle")
      simSuccess()
    }
}
