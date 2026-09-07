package rio

import spinal.core._
import spinal.lib._

/** The repository's existing SystemVerilog core, without algorithm changes. */
case class StockPifoBlackbox(config: PifoConfig) extends BlackBox {
  val io = new Bundle {
    val clk, rst = in Bool ()
    val pop_0 = in Bool ()
    val oprt_0 = in UInt (config.bitPort bits)
    val ovld_0 = out Bool ()
    val opri_0 = out UInt (config.bitPrio bits)
    val odout_0 = out UInt (config.bitData bits)
    val push_1, push_2, push_1_drop, push_2_drop = in Bool ()
    val uprt_1, uprt_2 = in UInt (config.bitPort bits)
    val upri_1, upri_2 = in UInt (config.bitPrio bits)
    val udin_1, udin_2 = in UInt (config.bitData bits)
    val odrop_vld_0 = out Bool ()
    val odrop_pri_0 = out UInt (config.bitPrio bits)
    val odrop_dout_0 = out UInt (config.bitData bits)
  }
  noIoPrefix()
  mapCurrentClockDomain(io.clk, io.rst)
  setDefinitionName("pifo")
  addGeneric("NUMPIFO", config.numPifo)
  addGeneric("BITPORT", config.bitPort)
  addGeneric("BITPRIO", config.bitPrio)
  addGeneric("BITDATA", config.bitData)
  addGeneric("PIFO_ID", 0)
  addRTLPath("hw/verilog/pifo.sv")
}

/** Experimental stock-core adapter for synthesis comparisons.
  *
  * Occupancy follows the PE contract: pop, accept push1 if space, then push2.
  * Per-port counters supply empty/drain signals absent from the stock interface.
  * These counters assume correct stock-core ordering; they cannot repair bugs
  * in the original pipelined index logic. See synthesis/STOCK_PIFO_RESULTS.md.
  */
class StockPifoRTL(config: PifoConfig) extends PifoCore(config) {
  require(isPow2(config.numPifo) && config.numPifo >= 2)
  private val countWidth = config.bitPifo + 1
  val core = StockPifoBlackbox(config)
  val count = Reg(UInt(countWidth bits)) init (0)
  val portCounts = Vec(Reg(UInt(countWidth bits)) init (0), 1 << config.bitPort)
  val portCount = portCounts(io.popRequest.port)
  val popExists = portCount =/= 0
  val popFire = io.popRequest.valid && popExists
  val afterPop = count - popFire.asUInt.resize(countWidth)
  val push1Fire = io.push1.valid && afterPop < config.numPifo
  val afterPush1 = afterPop + push1Fire.asUInt.resize(countWidth)
  val push2Fire = io.push2.valid && afterPush1 < config.numPifo
  count := afterPush1 + push2Fire.asUInt.resize(countWidth)

  for (port <- 0 until (1 << config.bitPort)) {
    val removed = popFire && io.popRequest.port === port
    val added1 = push1Fire && io.push1.port === port
    val added2 = push2Fire && io.push2.port === port
    portCounts(port) := portCounts(port) - removed.asUInt.resize(countWidth) +
      added1.asUInt.resize(countWidth) + added2.asUInt.resize(countWidth)
  }
  val samePortPush = (push1Fire && io.push1.port === io.popRequest.port) ||
    (push2Fire && io.push2.port === io.popRequest.port)
  io.popPortEmpty := !popExists
  io.portDrained.valid := RegNext(popFire && portCount === 1 && !samePortPush) init (False)
  io.portDrained.payload := RegNext(io.popRequest.port) init (0)
  io.popResponse.valid := RegNext(io.popRequest.valid) init (False)
  io.popResponse.port := RegNext(io.popRequest.port) init (0)
  io.popResponse.exist := core.io.ovld_0
  io.popResponse.priority := core.io.opri_0
  io.popResponse.data := core.io.odout_0

  // Flush the stock core's unreset command pipeline while reset is asserted.
  val running = !ClockDomain.current.isResetActive
  core.io.pop_0 := popFire && running
  core.io.oprt_0 := io.popRequest.port
  core.io.push_1 := push1Fire && running
  core.io.uprt_1 := io.push1.port
  core.io.upri_1 := io.push1.priority
  core.io.udin_1 := io.push1.data
  core.io.push_2 := push2Fire && running
  core.io.uprt_2 := io.push2.port
  core.io.upri_2 := io.push2.priority
  core.io.udin_2 := io.push2.data
  core.io.push_1_drop := False
  core.io.push_2_drop := False
}
