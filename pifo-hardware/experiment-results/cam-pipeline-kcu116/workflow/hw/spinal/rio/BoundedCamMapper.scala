package rio

import spinal.core._
import spinal.lib._

/** Exact-match lookup over a bounded set of nonzero key/value pairs.
  * Tags and valid bits are registers; each value bank has one synchronous read
  * port and one write port. Missing keys return zero, so writing zero deletes a
  * key without changing the zero-initialized dense mapper's observable contents.
  * A full bank accepts updates/deletes, but backpressures a new nonzero key.
  *
  * Replay owns two complete banks and writes only the shadow bank. The existing
  * shared control FIFO supplies the replay after publication. No key eviction or
  * separate command journal is used. Capacity counts distinct keys per bank,
  * including old/new tree contexts that must coexist.
  */
case class BoundedCamMapper(inputWidth: Int, outputWidth: Int, capacity: Int, replay: Boolean)
    extends ConfigurableMapper(inputWidth, outputWidth) {
  require(capacity >= 2 && BigInt(capacity) <= (BigInt(1) << inputWidth))
  private val addressWidth = log2Up(capacity)
  val activeBank: Bool = if (replay) RegInit(False) else False
  val requestedBank: Bool = if (replay) RegNextWhen(activeBank, io.readReq.valid) init(False) else False
  val deleting = io.writeReq.outputId === 0

  val banks = (0 until (if (replay) 2 else 1)).map { bankId => new Area {
    val tags = Vec.fill(capacity)(Reg(UInt(inputWidth bits)))
    val valid = Vec.fill(capacity)(RegInit(False))
    val values = Mem(UInt(outputWidth bits), capacity)
    val occupied = Bits(capacity bits)
    val readMatches = Bits(capacity bits)
    val writeMatches = Bits(capacity bits)
    for (i <- 0 until capacity) {
      occupied(i) := valid(i)
      readMatches(i) := valid(i) && tags(i) === io.readReq.payload
      writeMatches(i) := valid(i) && tags(i) === io.writeReq.inputId
    }
    val readHit = readMatches.orR
    val writeHit = writeMatches.orR
    // Assign bits explicitly: Vec.asBits builds a deeply nested concatenation
    // that can exhaust the elaborator stack for thousands of entries.
    val free = OHMasking.first(~occupied)
    val canWrite = writeHit || free.orR || deleting
    val selected = Mux(writeHit, writeMatches, free)
    val writeAddress = OHToUInt(selected).resize(addressWidth)
    val readAddress = OHToUInt(readMatches).resize(addressWidth)
    val readEnabled = if (!replay) True else if (bankId == 0) !activeBank else activeBank
    val writeEnabled = if (!replay) True else if (bankId == 0) activeBank else !activeBank
    val accepted = io.writeReq.valid && writeEnabled && canWrite

    // Explicit read-first semantics also cover a simultaneous ordinary-table
    // lookup and update of the same key. Insert/delete reads see pre-edge state.
    val readData = values.readSync(readAddress,
      io.readReq.valid && readEnabled && readHit, readUnderWrite = readFirst)
    val hitDelayed = RegNext(io.readReq.valid && readEnabled && readHit) init(False)
    val result = Mux(hitDelayed, readData, U(0, outputWidth bits))
    values.write(writeAddress, io.writeReq.outputId, accepted && !deleting)
    for (i <- 0 until capacity) {
      when(accepted && selected(i)) {
        valid(i) := !deleting
        when(!deleting) { tags(i) := io.writeReq.inputId }
      }
    }
  }}

  io.readRes.valid := RegNext(io.readReq.valid) init(False)
  if (replay) {
    io.readRes.payload := Mux(requestedBank, banks(1).result, banks(0).result)
    io.writeReq.ready := Mux(activeBank, banks(0).canWrite, banks(1).canWrite)
    when(io.commit) { activeBank := !activeBank }
  } else {
    io.readRes.payload := banks.head.result
    io.writeReq.ready := banks.head.canWrite
  }
  io.capacityBlocked := io.writeReq.valid && !io.writeReq.ready
  // Writes complete on their handshake edge. The ordered command FIFO holds a
  // full-table write before a later commit; readiness must not depend on the
  // current write-valid signal (which would loop through commit broadcasting).
  io.commitReady := True
}
