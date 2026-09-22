package rio

import spinal.core._
import spinal.lib._

/** One command RAM, read first for execution and then for mapper-write replay.
  * Executing a mapper write retains its slot through the following commit.
  * All later commands in that epoch remain in the same ring; replay skips
  * unbanked commands. One slot is reserved so a full epoch can still commit.
  */
case class ReplayControlFifo(config: EngineConfig) extends Component {
  val depth = config.commitQueueLength
  require(depth >= 2 && (depth & (depth - 1)) == 0)
  val addressWidth = log2Up(depth)
  val pointerWidth = addressWidth + 1
  val io = new Bundle {
    val push = slave Stream(ControlMessage(config))
    val pop = master Stream(ControlMessage(config))
    val replaying = out Bool()
    // Free slots for non-commit commands, excluding the reserved commit slot.
    val available = out UInt(log2Up(depth + 1) bits)
  }

  val storage = Mem(Bits(io.push.payload.getBitsWidth bits), depth)
  val writePointer = Reg(UInt(pointerWidth bits)) init(0)
  val readPointer = Reg(UInt(pointerWidth bits)) init(0)
  val releasePointer = Reg(UInt(pointerWidth bits)) init(0)
  val replayEnd = Reg(UInt(pointerWidth bits)) init(0)
  val retained = RegInit(False)
  val replaying = RegInit(False)
  val headValid = RegInit(False)

  val occupancy = writePointer - releasePointer
  val incomingCommit = io.push.command === ControlCommand.CommitMapper
  io.push.ready := !replaying && Mux(incomingCommit, occupancy < depth, occupancy < depth - 1)
  io.replaying := replaying
  io.available := 0
  when(occupancy < depth - 1) { io.available := U(depth - 1) - occupancy }

  storage.write(writePointer.resize(addressWidth), io.push.payload.asBits, io.push.fire)
  when(io.push.fire) { writePointer := writePointer + 1 }

  val head = ControlMessage(config)
  val mapper = head.command === ControlCommand.UpdateMapperPre ||
    head.command === ControlCommand.UpdateMapperPost
  val commit = head.command === ControlCommand.CommitMapper
  // Brain/state/front-rewrite commands and drain guards execute only on the first pass.
  val skip = replaying && !mapper
  io.pop.valid := headValid && !skip
  io.pop.payload := head
  val consume = headValid && (skip || io.pop.ready)

  val nextRead = UInt(pointerWidth bits)
  nextRead := readPointer
  when(consume) {
    nextRead := readPointer + 1
    when(replaying) {
      releasePointer := readPointer + 1
      when(readPointer + 1 === replayEnd) {
        // Reclaim the commit marker too, and resume commands after it.
        nextRead := replayEnd + 1
        releasePointer := replayEnd + 1
        replaying := False
        retained := False
      }
    } otherwise {
      when(commit && retained) {
        replayEnd := readPointer
        nextRead := releasePointer
        replaying := True
      } otherwise {
        when(mapper) {
          retained := True
        } elsewhen(!retained) {
          // An unbanked-only prefix needs no replay and can stream indefinitely.
          releasePointer := readPointer + 1
        }
      }
    }
  }

  // The RAM output register is also the held stream head. Keep both the read
  // address and output stable under backpressure. Never read a just-arriving
  // entry on its write cycle, so no read-during-write behavior is required.
  val advance = !headValid || consume
  val readEnable = advance && (nextRead =/= writePointer)
  head.assignFromBits(storage.readSync(nextRead.resize(addressWidth), readEnable))
  when(advance) {
    readPointer := nextRead
    headValid := readEnable
  }
}
