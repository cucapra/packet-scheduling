package rio

import spinal.core._
import spinal.lib._

/** Sorted-register PIFO with deterministic simultaneous pop/push state updates.
  *
  * The original PifoRTL accumulates shift codes for all operations. A pop can consequently overwrite a newly inserted
  * entry when push and pop target overlapping positions. This implementation applies operations in an explicit order:
  * pop, push1, then push2. Equal priorities remain stable because insertion occurs after existing equal-ranked entries.
  */
class ConcurrentPifoRTL(config: PifoConfig) extends Component {
  val io = new Bundle {
    val push1 = slave(Flow(PifoEntry(config)))
    val push2 = slave(Flow(PifoEntry(config)))
    val popRequest = slave(Flow(PifoPopInterface(config)))
    val popResponse = master(Flow(PifoPopResponse(config)))
    val popPortEmpty = out Bool ()
    // Pulses when a successful pop leaves its virtual PIFO with no entries.
    val portDrained = master(Flow(UInt(config.bitPort bits)))
  }

  private val countWidth = config.bitPifo + 1
  private val pifoArray = Vec(Reg(PifoEntry(config)), config.numPifo)
  private val pifoCount = Reg(UInt(countWidth bits)) init (0)

  private def firstPosition(entries: Vec[PifoEntry], count: UInt)(matches: PifoEntry => Bool): (Bool, UInt) = {
    val matchBits = Vec(Bool(), config.numPifo)
    entries.zip(matchBits).zipWithIndex.foreach { case ((entry, bit), index) =>
      bit := Mux(U(index, countWidth bits) < count, matches(entry), False)
    }
    val encoder = PriorityEncoderLogBlackbox(config.numPifo)
    encoder.io.decode := matchBits.asBits
    (encoder.io.valid, encoder.io.encode)
  }

  private def insertionPosition(entries: Vec[PifoEntry], count: UInt, priority: UInt): UInt = {
    val matchBits = Vec(Bool(), config.numPifo)
    entries.zip(matchBits).zipWithIndex.foreach { case ((entry, bit), index) =>
      bit := Mux(U(index, countWidth bits) < count, entry.priority > priority, True)
    }
    val encoder = PriorityEncoderLogBlackbox(config.numPifo)
    encoder.io.decode := matchBits.asBits
    encoder.io.encode
  }

  val (popExists, popPosition) = firstPosition(pifoArray, pifoCount)(_.port === io.popRequest.port)
  io.popPortEmpty := !popExists
  val popFire = io.popRequest.valid && popExists
  val otherPortMatches = Vec(Bool(), config.numPifo)
  pifoArray.zip(otherPortMatches).zipWithIndex.foreach { case ((entry, matches), index) =>
    matches :=
      U(index, countWidth bits) < pifoCount &&
        entry.port === io.popRequest.port &&
        U(index, config.bitPifo bits) =/= popPosition
  }
  val popWasLastForPort = popExists && !otherPortMatches.asBits.orR

  val countAfterPop = UInt(countWidth bits)
  countAfterPop := pifoCount
  when(popFire) {
    countAfterPop := pifoCount - 1
  }

  val afterPop = Vec(PifoEntry(config), config.numPifo)
  for (index <- 0 until config.numPifo) {
    afterPop(index) := pifoArray(index)
    if (index < config.numPifo - 1) {
      when(
        popFire && U(index, countWidth bits) >= popPosition.resize(countWidth) &&
          U(index, countWidth bits) < countAfterPop
      ) {
        afterPop(index) := pifoArray(index + 1)
      }
    }
  }

  val push1Position = insertionPosition(afterPop, countAfterPop, io.push1.priority)
  val push1Fire = io.push1.valid && countAfterPop < config.numPifo
  val countAfterPush1 = UInt(countWidth bits)
  countAfterPush1 := countAfterPop
  when(push1Fire) {
    countAfterPush1 := countAfterPop + 1
  }

  val afterPush1 = Vec(PifoEntry(config), config.numPifo)
  for (index <- 0 until config.numPifo) {
    afterPush1(index) := afterPop(index)
    if (index > 0) {
      when(push1Fire && U(index, config.bitPifo bits) > push1Position) {
        afterPush1(index) := afterPop(index - 1)
      }
    }
    when(push1Fire && U(index, config.bitPifo bits) === push1Position) {
      afterPush1(index) := io.push1.payload
    }
  }

  val push2Position = insertionPosition(afterPush1, countAfterPush1, io.push2.priority)
  val push2Fire = io.push2.valid && countAfterPush1 < config.numPifo
  val countAfterPush2 = UInt(countWidth bits)
  countAfterPush2 := countAfterPush1
  when(push2Fire) {
    countAfterPush2 := countAfterPush1 + 1
  }

  val afterPush2 = Vec(PifoEntry(config), config.numPifo)
  for (index <- 0 until config.numPifo) {
    afterPush2(index) := afterPush1(index)
    if (index > 0) {
      when(push2Fire && U(index, config.bitPifo bits) > push2Position) {
        afterPush2(index) := afterPush1(index - 1)
      }
    }
    when(push2Fire && U(index, config.bitPifo bits) === push2Position) {
      afterPush2(index) := io.push2.payload
    }
    pifoArray(index) := afterPush2(index)
  }
  pifoCount := countAfterPush2

  // A simultaneous accepted push to the same port keeps that port non-empty,
  // so it must not activate a drain rewrite.
  val samePortPush =
    (push1Fire && io.push1.port === io.popRequest.port) ||
      (push2Fire && io.push2.port === io.popRequest.port)
  val portDrained = popFire && popWasLastForPort && !samePortPush
  io.portDrained.valid := RegNext(portDrained) init (False)
  io.portDrained.payload := RegNext(io.popRequest.port) init (0)

  io.popResponse.valid := RegNext(io.popRequest.valid)
  io.popResponse.port := RegNext(io.popRequest.port)
  io.popResponse.exist := RegNext(popExists)
  io.popResponse.data := RegNext(pifoArray(popPosition).data)
  io.popResponse.priority := RegNext(pifoArray(popPosition).priority)
}
