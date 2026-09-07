package rio

import spinal.core._
import spinal.lib._

/** Interface required by the PE, including front-rewrite drain detection. */
abstract class PifoCore(config: PifoConfig) extends Component {
  val io = new Bundle {
    val push1 = slave(Flow(PifoEntry(config)))
    val push2 = slave(Flow(PifoEntry(config)))
    val popRequest = slave(Flow(PifoPopInterface(config)))
    val popResponse = master(Flow(PifoPopResponse(config)))
    val popPortEmpty = out Bool ()
    val portDrained = master(Flow(UInt(config.bitPort bits)))
  }
}
