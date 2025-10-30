package rio

import spinal.core._
import spinal.lib._

// flow = vPIFOs + FlowId

case class EngineConfig (
    numEngines : Int,
    numVPIFOs : Int,
    maxPacketPriority: Int,
    fifoDepth: Int,
    prefetchBufferDepth: Int
) {
    def vpifoIdWidth = log2Up(numVPIFOs)
    def numFlows = numVPIFOs * numEngines
    def flowIdWidth = log2Up(numFlows)
    def engineIdWidth = log2Up(numEngines)
}

object EngineConfig {
  implicit def toFlowPifoConfig(pifoConfig: EngineConfig): PifoConfig =
    PifoConfig(
      numPifo = pifoConfig.numVPIFOs * pifoConfig.fifoDepth,
      bitPort = pifoConfig.vpifoIdWidth,
      bitPrio = log2Up(pifoConfig.maxPacketPriority),
      bitData = pifoConfig.flowIdWidth
    )
}

case class MapperUpdater(numInputs : Int, numOutputs : Int) extends Bundle {
  val inputId = UInt(log2Up(numInputs) bits)
  val outputId = UInt(log2Up(numOutputs) bits)
}

case class Mapper(numInputs : Int, numOutputs : Int) extends Component {
  val inputWidth = log2Up(numInputs)
  val outputWidth = log2Up(numOutputs)
  def updater = MapperUpdater(numInputs, numOutputs)

  val io = new Bundle {
    val readReq = slave Flow(UInt(log2Up(numInputs) bits))
    val readRes = master Flow(UInt(log2Up(numOutputs) bits))

    val writeReq = slave Flow(updater)
  }

  val ram = Mem(UInt(outputWidth bits), numInputs)

  // read logic
  ram.flowReadSync(io.readReq) >> io.readRes

  // write logic
  ram.writePort().translateFrom(io.writeReq) { (to, from) =>
    to.address := from.inputId
    to.data := from.outputId
  }
}

case class BrainInput(config : EngineConfig) extends Bundle {
  val vpifoId = UInt(config.vpifoIdWidth bits)
  val flowId = UInt(config.flowIdWidth bits)
}

case class PIFOBrain(config : EngineConfig) extends Component {
  val io = new Bundle {
    val in = slave Stream(BrainInput(config))
    val out = master Stream(PifoEntry(config))

    val control = new Bundle {
    }
  }

  val inHeads = StreamFork(io.in, 4)

  val engineMapper = Mapper(config.numVPIFOs, 16)
  inHeads(0).map(_.vpifoId).toFlow >> engineMapper.io.readReq


  val engineCAM = Mapper(config.numVPIFOs * config.numFlows, 16)
  inHeads(1).map { data =>
    data.vpifoId @@ data.flowId
  }.toFlow >> engineCAM.io.readReq

  val brainStateMem = Mapper(config.numVPIFOs, 1024)
  inHeads(2).map { _.vpifoId }.toFlow >> brainStateMem.io.readReq

  val engineStream = StreamJoin(Seq(
    engineMapper.io.readRes.toStream,
    engineCAM.io.readRes.toStream,
    brainStateMem.io.readRes.toStream,
    inHeads(3)
  )).translateWith(
    new Bundle {
      val pifoId = inHeads(3).payload.vpifoId
      val flowId = inHeads(3).payload.flowId
      val engineId = engineMapper.io.readRes.payload
      val flowState = engineCAM.io.readRes.payload
      val brainState = brainStateMem.io.readRes.payload
    }
  )
  

  // Brain types for the PIFO brain engine
  object BrainType extends SpinalEnum {
    val NOP, SP, RR, FIFO = newElement()
  }

  // Engine Logic
  val outStream = engineStream.map { data =>
    val entry = PifoEntry(config)
    entry.port := data.pifoId
    
    switch(data.engineId) {
      // strict priority
      is(BrainType.SP) {
        entry.priority := data.flowState.resized
      }

      // round robin
      is(BrainType.RR) {

      }

      // FIFO
      is(BrainType.FIFO) {

      }

      default {
        // invalid entry
        entry.priority := 0
      }
    }

    entry
  }

  // priority should not be zero
  io.out << outStream.throwWhen(outStream.payload.priority === 0)
}

case class PifoMessage(config: EngineConfig) extends Bundle {
  val engineId = UInt(config.engineIdWidth bits)
  val vPifoId = UInt(config.vpifoIdWidth bits)

  def flowId : UInt = engineId @@ vPifoId
}

object PifoMessage {
  def fromData(config : EngineConfig, data : UInt) : PifoMessage = {
    val msg = PifoMessage(config)
    msg.engineId := data(config.engineIdWidth - 1 downto 0)
    msg.vPifoId := data(config.engineIdWidth + config.vpifoIdWidth - 1 downto config.engineIdWidth)
    msg
  }
}

case class PifoEngine(config : EngineConfig) extends Component {
  val io = new Bundle {
    val enqueRequest = slave Stream(PifoMessage(config))
    val dequeueRequest = slave Stream(PifoMessage(config))

    val dequeueResponse = master Stream(PifoMessage(config))

    // control signals
    val control = slave Stream(ControlMessage(config))
  }

  // PIFO
  val pifos = new PifoRTL(config)

  // enque logic
  // enqueMapper maps flowIds to VPIFO ids
  val enque = new Area {
    val (mapperRead, flowIdStream) = StreamFork2(io.enqueRequest)

    val enqueMapper = Mapper(config.numFlows, config.numVPIFOs)
    enqueMapper.io.readReq << mapperRead.map(_.flowId).toFlow

    val brainInput = Stream(BrainInput(config))
    StreamJoin(enqueMapper.io.readRes.toStream, flowIdStream)
      .translateInto(brainInput) { (to, from) =>
        to.vpifoId := from._1
        to.flowId := from._2.flowId
      }
    
    // brain takes (vpid, flowid) to PIFOEntry(priority, flowid)
    // each VPIFO has its own brain
    val brain = PIFOBrain(config)
    brain.io.in << brainInput
    

    // flow PIFO will give the result
    pifos.io.push1 << brain.io.out.toFlow
  }

  val deque = new Area {
    // dequeue also need a mapper, reinterpret vpifoId if needed
    val dequeMapper = Mapper(config.numVPIFOs, config.numVPIFOs)

    pifos.io.popRequest.translateFrom(dequeMapper.io.readRes) { _.port := _ }

    pifos.io.popResponse
      .throwWhen(!pifos.io.popResponse.exist)
      .map(resp => PifoMessage.fromData(config, resp.data))
      .toStream >> io.dequeueResponse
  }

  val control = new Area {
    // control logic to write the mappers and brain state
    val controlHeads = StreamFork(io.control, 5)
    controlHeads(0)
      .takeWhen(controlHeads(0).payload.command == ControlCommand.MODIFY_MAPPING && controlHeads(0).mappingId == MappingId.InputMapper)
      .translateInto(Stream(enque.enqueMapper.updater)) { (to, from) =>
        to.inputId := from.flowId
        to.outputId := from.vPifoId
      }.toFlow >> enque.enqueMapper.io.writeReq

    // continue with other control paths...

  }
}