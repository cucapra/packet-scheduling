package rio

import spinal.core._
import spinal.lib._

/** Explicit synthesis boundary: the RIO side drives pushes and pop requests.
  * Responses remain unconstrained inputs, preserving all surrounding logic.
  */
case class PifoBoundary(config: PifoConfig) extends Bundle with IMasterSlave {
  val push1 = Flow(PifoEntry(config))
  val push2 = Flow(PifoEntry(config))
  val popRequest = Flow(PifoPopInterface(config))
  val popResponse = Flow(PifoPopResponse(config))
  val popPortEmpty = Bool()
  val portDrained = Flow(UInt(config.bitPort bits))

  override def asMaster(): Unit = {
    master(push1, push2, popRequest)
    slave(popResponse, portDrained)
    in(popPortEmpty)
  }
}

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

/** Wiring only; contains no sorter, storage, occupancy, or drain detector. */
class ExternalPifoCore(config: PifoConfig) extends PifoCore(config) {
  val boundary = master(PifoBoundary(config))
  boundary.push1 << io.push1
  boundary.push2 << io.push2
  boundary.popRequest << io.popRequest
  io.popResponse << boundary.popResponse
  io.popPortEmpty := boundary.popPortEmpty
  io.portDrained << boundary.portDrained
}
