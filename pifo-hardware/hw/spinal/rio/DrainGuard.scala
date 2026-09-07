package rio

import spinal.core._
import spinal.lib._

/** Remember the same last-successful-pop notifications used by front rewrite.
  * Empty at reset; a real accepted push clears the notification on FIFO reuse.
  * Guards are for quiesced FIFOs: the producer must stop inserting old tokens
  * before reclamation. They do not themselves stop packet admission.
  */
case class DrainGuard(config: EngineConfig) extends Component {
  val io = new Bundle {
    val nearlyDrained = Vec(slave(Flow(UInt(config.vpifoIdWidth bits))), config.numEngines)
    val pushed = Vec(Vec(slave(Flow(UInt(config.vpifoIdWidth bits))), 2), config.numEngines)
    val engineId = in UInt (config.engineIdWidth bits)
    val vPifoId = in UInt (config.vpifoIdWidth bits)
    val satisfied = out Bool ()
  }

  val emptyPifos = Vec.fill(config.numEngines)(Vec.fill(config.numVPIFOs)(RegInit(True)))
  val readyByEngine = Vec(Bool(), config.numEngines)
  for (engine <- 0 until config.numEngines) {
    val empty = emptyPifos(engine)
    when(io.nearlyDrained(engine).valid) {
      empty(io.nearlyDrained(engine).payload) := True
    }
    for (push <- io.pushed(engine)) {
      when(push.valid) { empty(push.payload) := False }
    }
    // The notification precedes the last token's post-mapper lookup. Give that
    // lookup a settling cycle before allowing any reclamation command through.
    val settled = RegNext(empty.asBits) init (B((BigInt(1) << config.numVPIFOs) - 1, config.numVPIFOs bits))
    val refilling = io.pushed(engine).map(push => push.valid && push.payload === io.vPifoId).reduce(_ || _)
    readyByEngine(engine) := empty(io.vPifoId) && settled(io.vPifoId) && !refilling
  }
  io.satisfied := False
  for (engine <- 0 until config.numEngines) {
    when(io.engineId === engine + 1) { io.satisfied := readyByEngine(engine) }
  }
}
