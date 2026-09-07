package rio

import spinal.core._
import spinal.lib._

/** Packetized hierarchical WFQ for equal-sized packets. A group is a child arm,
  * not necessarily a flow. Quantum = common scale / arm weight, set by compiler.
  * Ranks freeze at admission. Reweight changes quantum without resetting finish.
  */
case class WeightedRanker(config: EngineConfig) extends Component {
  private val width = config.vpifoIdWidth
  private val entries = 1 << (2 * width)
  val io = new Bundle {
    val port = in UInt (width bits)
    val flow = in UInt (width bits)
    val accept = in Bool ()
    val rank = out UInt (config.bitPrio bits)
    val pop = slave Flow (PifoPopResponse(config))
    val control = slave Flow (ControlMessage(config))
    val commit = in Bool ()
  }
  val groups = Vec.fill(entries)(Reg(UInt(width bits)) init(0))
  val pendingGroups = Vec.fill(entries)(Reg(UInt(width bits)) init(0))
  val quanta = Vec.fill(entries)(Reg(UInt(config.bitPrio bits)) init(1))
  val pendingQuanta = Vec.fill(entries)(Reg(UInt(config.bitPrio bits)) init(1))
  val finishes = Mem(UInt(config.bitPrio bits), entries) init(Seq.fill(entries)(BigInt(0)))
  val virtualTimes = Vec.fill(config.numVPIFOs)(Reg(UInt(config.bitPrio bits)) init(0))
  val group = groups(io.port @@ io.flow)
  val key = io.port @@ group
  val previous = finishes.readAsync(key)
  val virtualTime = virtualTimes(io.port)
  val start = Mux(previous > virtualTime, previous, virtualTime)
  io.rank := start + quanta(key)
  when(io.accept) {
    assert(io.rank > start, "weighted rank overflow or zero quantum")
    finishes.write(key, io.rank)
  }
  when(io.pop.valid && io.pop.exist) { virtualTimes(io.pop.port) := io.pop.priority }
  val controlKey = io.control.vPifoId @@ io.control.flowId.resize(width)
  when(io.control.valid && io.control.command === ControlCommand.UpdateRankGroup) {
    pendingGroups(controlKey) := io.control.data.resized
  }
  when(io.control.valid && io.control.command === ControlCommand.UpdateRankQuantum) {
    pendingQuanta(controlKey) := io.control.data.resized
  }
  when(io.commit) {
    groups := pendingGroups
    quanta := pendingQuanta
  }
}
