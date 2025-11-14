package rio

import scala.collection.mutable.ArrayBuffer

import spinal.core._
import spinal.lib._
import scala.collection.mutable

// Configuration case class to match the Verilog PIFO parameters
case class PifoConfig(
  numPifo: Int = 64,
  bitPort: Int = 8,
  bitPrio: Int = 16,
  bitData: Int = 32,
) {
  val bitPifo = log2Up(numPifo)
}

// Data type for PIFO entries
case class PifoData(config: PifoConfig) extends Bundle {
  val priority = UInt(config.bitPrio bits)
  val data = UInt(config.bitData bits)
}

// Pop interface bundle
case class PifoPopInterface(config: PifoConfig) extends Bundle {
  val port = UInt(config.bitPort bits)
}

// Pop response bundle
case class PifoPopResponse(config: PifoConfig) extends Bundle {
  val exist = Bool()
  val priority = UInt(config.bitPrio bits)
  val data = UInt(config.bitData bits)
  val port = UInt(config.bitPort bits)
}

// Push interface bundle
case class PifoEntry(config: PifoConfig) extends Bundle {
  val priority = UInt(config.bitPrio bits)
  val data     = UInt(config.bitData bits)
  val port     = UInt(config.bitPort bits)
}

// SpinalHDL blackbox wrapper for the pifo Verilog module
case class PifoBlackbox(config: PifoConfig = PifoConfig()) extends BlackBox {
  
  // Define the IO bundle to match the Verilog interface exactly
  val io = new Bundle {
    // Pop interface (single pop port)
    val pop_0 = in Bool()
    val oprt_0 = in UInt(config.bitPort bits)
    val ovld_0 = out Bool()
    val opri_0 = out UInt(config.bitPrio bits)
    val odout_0 = out UInt(config.bitData bits)
    
    // Push interface 1
    val push_1 = in Bool()
    val uprt_1 = in UInt(config.bitPort bits)
    val upri_1 = in UInt(config.bitPrio bits)
    val udin_1 = in UInt(config.bitData bits)
    
    // Push interface 2  
    val push_2 = in Bool()
    val uprt_2 = in UInt(config.bitPort bits)
    val upri_2 = in UInt(config.bitPrio bits)
    val udin_2 = in UInt(config.bitData bits)
  }
  
  // Ensure port names match exactly with the Verilog module
  noIoPrefix()
  
  // Clock and reset mapping
  val clk = in Bool()
  val rst = in Bool()
  mapCurrentClockDomain(clk, rst)
  
  // Set the Verilog module name
  setDefinitionName("pifo")
  
  // Add Verilog parameters
  addGeneric("NUMPIFO", config.numPifo)
  addGeneric("BITPORT", config.bitPort)
  addGeneric("BITPRIO", config.bitPrio)
  addGeneric("BITDATA", config.bitData)
  
  // Add the appropriate Verilog file based on useDropFeature flag
  addRTLPath("hw/verilog/pifo_orig.v")
}

// Wrapper class with Flow interfaces as requested
class Pifo(config: PifoConfig = PifoConfig()) extends Component {
  
  val io = new Bundle {
    // Pop interface - single flow
    val popRequest = slave(Flow(PifoPopInterface(config)))
    val popResponse = master(Flow(PifoPopResponse(config)))
    
    // Push interfaces - 1 flow each as requested
    val push1 = slave(Flow(PifoEntry(config)))
    val push2 = slave(Flow(PifoEntry(config)))
  }
  
  // Instantiate the blackbox
  val pifo = PifoBlackbox(config)
  
  // Connect pop interface directly
  pifo.io.pop_0 := io.popRequest.valid
  pifo.io.oprt_0 := io.popRequest.port
  
  val popValid = Reg(Bool()) init(False)
  popValid := pifo.io.pop_0

  io.popResponse.valid := popValid
  io.popResponse.exist := pifo.io.ovld_0
  io.popResponse.priority := pifo.io.opri_0
  io.popResponse.data := pifo.io.odout_0
  
  // Connect push interfaces directly (1 flow each)
  pifo.io.push_1 := io.push1.valid
  pifo.io.uprt_1 := io.push1.port
  pifo.io.upri_1 := io.push1.priority
  pifo.io.udin_1 := io.push1.data
  
  pifo.io.push_2 := io.push2.valid
  pifo.io.uprt_2 := io.push2.port
  pifo.io.upri_2 := io.push2.priority
  pifo.io.udin_2 := io.push2.data
}

// Helper case class for PIFO entries

// SpinalHDL RTL implementation of PIFO
class PifoRTL(config: PifoConfig) extends Component {
  
  val io = new Bundle {
    // Push interfaces - 2 input ports
    val push1 = slave(Flow(PifoEntry(config)))
    val push2 = slave(Flow(PifoEntry(config)))
    
    // Pop request interface - specifies which port to pop from
    val popRequest = slave(Flow(PifoPopInterface(config)))
    val popResponse = master(Flow(PifoPopResponse(config)))
  }
  
  // Internal PIFO storage
  val pifoArray = Vec(Reg(PifoEntry(config)), config.numPifo)
  val pifoCount = Reg(UInt(config.bitPifo + 1 bits)) init(0)
  
  // Find insertion position for a packet based on priority
  def findFirstPosition(defaultValue : Bool = False)(F : PifoEntry => Bool): (Bool, UInt) = {
    val bits = Vec(Bool(), config.numPifo)
    (pifoArray zip bits).zipWithIndex.foreach { case ((idx, bit), i) =>
      // TODO(zhiyuang): check this condition
      bit := Mux(U(i) < pifoCount, F(idx), defaultValue)
    }
    val encoder = PriorityEncoderLogBlackbox(config.numPifo)
    encoder.io.decode := bits.asBits
    (encoder.io.valid, encoder.io.encode)
  }

  // Array update logic
  var nextArray = ArrayBuffer.fill(config.numPifo)(UInt(2 bits))
  for (i <- 0 until config.numPifo) { nextArray(i) := 0 }
  var nextCount = CombInit(pifoCount)

  def prepareShift(start : UInt, offset : UInt) = {
    for(i <- 0 until config.numPifo) {
      when(U(i) >= start && U(i) < config.numPifo) {
        nextArray(i) \= nextArray(i) + offset
      }
    }
  }
  
  // Find pop position based on requested port
  val (popExists, popPosition) = findFirstPosition()(_.port === io.popRequest.port)

  when (io.popRequest.valid && popExists) {
    prepareShift(popPosition, 3)
    nextCount \= nextCount - 1
  }

  val (_, pos1) = findFirstPosition(True) { _.priority > io.push1.priority }
  val (_, pos2) = findFirstPosition(True) { _.priority > io.push2.priority }

  var adjustedPos1 = CombInit(pos1)
  when(popPosition < pos1 && io.popRequest.valid) { adjustedPos1 \= adjustedPos1 - 1 }
  when(pos2 < pos1 && io.push2.valid) { adjustedPos1 \= adjustedPos1 + 1 }

  var adjustedPos2 = CombInit(pos2)
  when(popPosition < pos2 && io.popRequest.valid) { adjustedPos2 \= adjustedPos2 - 1 }
  when(pos1 < pos2 && io.push1.valid) { adjustedPos2 \= adjustedPos2 + 1 }
  when(pos1 === pos2 && io.push1.valid) { adjustedPos2 \= adjustedPos2 + 1 }

  when (io.push1.valid && nextCount < config.numPifo) {
    prepareShift(adjustedPos1 + 1, 1)
    nextCount \= nextCount + 1
  }
  when (io.push2.valid && nextCount < config.numPifo) {
    prepareShift(adjustedPos2 + 1, 1)
    nextCount \= nextCount + 1
  }

  // TODO(zhiyuang): convert this to a mux on shift regs
  for (i <- 0 until config.numPifo) {
    var orig = CombInit(pifoArray(i))
    when (adjustedPos1 === U(i) && io.push1.valid) {
      orig := io.push1.payload
    } elsewhen (adjustedPos2 === U(i) && io.push2.valid) {
      orig := io.push2.payload
    }
    pifoArray(i) := orig

    if (i > 0) {
      when (nextArray(i) === 1) {
        pifoArray(i) := pifoArray(i - 1)
      }
    }

    if (i > 1) {
      when (nextArray(i) === 2) {
        pifoArray(i) := pifoArray(i - 2)
      }
    }

    if (i < config.numPifo - 1) {
      when (nextArray(i) === 3) {
        pifoArray(i) := pifoArray(i + 1)
      }
    }
  }

  // output next cycle
  io.popResponse.valid := RegNext(io.popRequest.valid)
  io.popResponse.port := RegNext(io.popRequest.port)
  io.popResponse.exist := RegNext(popExists)
  io.popResponse.data := RegNext(pifoArray(popPosition).data)
  io.popResponse.priority := RegNext(pifoArray(popPosition).priority)

  pifoCount := nextCount
}

// SpinalHDL blackbox wrapper for priority_encode_log.v
case class PriorityEncoderLogBlackbox(width: Int) extends BlackBox {
  assert(isPow2(width), "Width must be a power of 2")
  val logWidth = log2Up(width)

  addGeneric("width", width)
  addGeneric("log_width", logWidth)
  
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val decode = in Bits(width bits)
    val encode = out UInt(logWidth bits)
    val valid = out Bool()
  }
  
  noIoPrefix()
  
  // Clock and reset mapping
  mapCurrentClockDomain(io.clk, io.rst)
  
  // Set the Verilog module name
  setDefinitionName("priority_encode_log")
  
  // Add the Verilog file path
  addRTLPath("hw/verilog/priority_encode_log.v")
}

