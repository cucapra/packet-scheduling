package rio

import spinal.core._
import spinal.lib._

/** Numeric initialization avoids elaborating one UInt literal per RAM word.
  * The resulting all-zero memory contents and synthesized hardware are unchanged.
  */
object MapperMemory {
  private val zero = BigInt(0)
  def zeroed(width: Int, depth: Int): Mem[UInt] =
    Mem(UInt(width bits), depth).initBigInt(scala.collection.immutable.ArraySeq.fill(depth)(zero))
}

/** Common lookup/configuration interface for atomic and ordinary mapper RAMs. */
abstract class ConfigurableMapper(inputWidth: Int, outputWidth: Int) extends Component {
  require(inputWidth > 0 && outputWidth > 0)
  def updater = MapperUpdater(inputWidth, outputWidth)
  val io = new Bundle {
    val readReq = slave Flow (UInt(inputWidth bits))
    val readRes = master Flow (UInt(outputWidth bits))
    val writeReq = slave Stream (updater)
    val commit = in Bool ()
    val commitReady = out Bool ()
  }
  // Retain the observation point used by the existing atomic-visibility tests.
  val activeBank: Bool
}

/** Ordinary single-bank lookup table for the non-atomic hardware baseline.
  * Writes take effect directly; commit is accepted without changing any state.
  * Packet reads have the same one-cycle latency as TransactionalMapper.
  */
case class DirectMapper(inputWidth: Int, outputWidth: Int)
    extends ConfigurableMapper(inputWidth, outputWidth) {
  val ram = MapperMemory.zeroed(outputWidth, 1 << inputWidth)
  val activeBank = False
  io.readRes.payload := ram.readSync(io.readReq.payload, io.readReq.valid)
  io.readRes.valid := RegNext(io.readReq.valid) init (False)
  io.writeReq.ready := True
  io.commitReady := True
  ram.write(io.writeReq.payload.inputId, io.writeReq.payload.outputId, io.writeReq.fire)
}

/** Double-bank mapper synchronized by command replay in the mesh controller.
  * Each bank has exactly one packet-read port and one configuration-write port.
  * The controller must replay staged writes after a swap before issuing another
  * commit. Standalone users of this component must provide that same protocol.
  */
case class ReplayMapper(inputWidth: Int, outputWidth: Int)
    extends ConfigurableMapper(inputWidth, outputWidth) {
  val banks = Seq.fill(2)(MapperMemory.zeroed(outputWidth, 1 << inputWidth))
  val activeBank = RegInit(False)
  val requestedBank = RegInit(False)
  val bankRead = Seq(
    banks(0).readSync(io.readReq.payload, io.readReq.valid && !activeBank),
    banks(1).readSync(io.readReq.payload, io.readReq.valid && activeBank)
  )
  when(io.readReq.valid) { requestedBank := activeBank }
  io.readRes.valid := RegNext(io.readReq.valid) init (False)
  io.readRes.payload := Mux(requestedBank, bankRead(1), bankRead(0))
  io.writeReq.ready := True
  io.commitReady := True
  banks(0).write(io.writeReq.inputId, io.writeReq.outputId, io.writeReq.fire && activeBank)
  banks(1).write(io.writeReq.inputId, io.writeReq.outputId, io.writeReq.fire && !activeBank)
  when(io.commit) { activeBank := !activeBank }
}
