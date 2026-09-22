package rio

import spinal.core._
import spinal.lib._

/** Packet lookups read the active bank; configuration writes select the other
  * bank. The mesh controller replays those writes after commit before allowing
  * another epoch. Standalone users must provide the same replay protocol.
  */
case class ReplayMapper(inputWidth: Int, outputWidth: Int) extends Component {
  require(inputWidth > 0 && outputWidth > 0)
  val numInputs = 1 << inputWidth
  val io = new Bundle {
    val readReq = slave Flow(UInt(inputWidth bits))
    val readRes = master Flow(UInt(outputWidth bits))
    val writeReq = slave Stream(MapperUpdater(inputWidth, outputWidth))
    val commit = in Bool()
    val commitReady = out Bool()
  }

  val banks = Seq.fill(2)(Mem(UInt(outputWidth bits), numInputs) init(Seq.fill(numInputs)(0)))
  val activeBank = RegInit(False)
  val requestedBank = RegInit(False)
  val bankRead = Seq(
    banks(0).readSync(io.readReq.payload, io.readReq.valid && !activeBank),
    banks(1).readSync(io.readReq.payload, io.readReq.valid && activeBank)
  )
  when(io.readReq.valid) { requestedBank := activeBank }
  io.readRes.valid := RegNext(io.readReq.valid) init(False)
  io.readRes.payload := Mux(requestedBank, bankRead(1), bankRead(0))
  io.writeReq.ready := True
  io.commitReady := True
  banks(0).write(io.writeReq.inputId, io.writeReq.outputId, io.writeReq.fire && activeBank)
  banks(1).write(io.writeReq.inputId, io.writeReq.outputId, io.writeReq.fire && !activeBank)
  when(io.commit) { activeBank := !activeBank }
}
