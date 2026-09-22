package rio

import spinal.core._
import spinal.lib._

/** Evaluation-only whole-PE move. Entries retain their port, token and rank.
  * The source is cleared only after every entry has reached an empty destination.
  * Mappers are configured separately: copied trees are frozen, drain-only trees.
  */
case class PifoCopyController(config: EngineConfig) extends Component {
  val io = new Bundle {
    val request = slave Stream (new Bundle {
      val source = UInt(config.engineIdWidth bits)
      val target = UInt(config.engineIdWidth bits)
    })
    val index = out UInt (config.bitPifo bits)
    val entries = Vec(slave Flow (PifoEntry(config)), config.numEngines)
    val insert = Vec(master Stream (PifoEntry(config)), config.numEngines)
    val clear = out Bits (config.numEngines bits)
    val empty = in Bits (config.numEngines bits)
    val busy = out Bool ()
    val done = out Bool ()
    val copied = out UInt ((config.bitPifo + 1) bits)
  }
  val busy = RegInit(False)
  private val indexWidth = log2Up(config.numEngines)
  val source = Reg(UInt(indexWidth bits)) init (0)
  val target = Reg(UInt(indexWidth bits)) init (0)
  val copied = Reg(UInt((config.bitPifo + 1) bits)) init (0)
  val selected = io.entries(source.resized)
  io.index := copied.resized
  io.copied := copied
  io.busy := busy
  io.request.ready := !busy
  io.clear := 0
  io.done := False
  for (i <- 0 until config.numEngines) {
    io.insert(i).valid := busy && selected.valid && copied < config.numVPIFOs * config.fifoDepth && target === i
    io.insert(i).payload := selected.payload
  }
  when(io.request.fire) {
    assert(io.request.source =/= io.request.target, "copy requires different PEs")
    assert(io.request.source >= 1 && io.request.source <= config.numEngines, "invalid copy source")
    assert(io.request.target >= 1 && io.request.target <= config.numEngines, "invalid copy target")
    assert(io.empty((io.request.target - 1).resize(indexWidth)), "copy destination must be empty")
    source := (io.request.source - 1).resized
    target := (io.request.target - 1).resized
    copied := 0
    busy := True
  }
  when(busy) {
    when(!selected.valid || copied === config.numVPIFOs * config.fifoDepth) {
      io.clear(source.resized) := True
      io.done := True
      busy := False
    } elsewhen(io.insert(target.resized).fire) {
      copied := copied + 1
    }
  }
}
