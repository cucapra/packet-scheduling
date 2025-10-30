package rio

import spinal.core._
import spinal.lib._

case class MessageCrossBar(config : EngineConfig) extends Component {
    val numPorts = config.numEngines + 1
    val io = new Bundle {
        val inputs = Vec(slave Stream(PifoMessage(config)), numPorts)
        val outputs = Vec(master Stream(PifoMessage(config)), numPorts)
    }

    val fanouts = io.inputs.map { in =>
        StreamDemux(in, in.payload.engineId, numPorts)
    }

    for(i <- 0 until numPorts) {
        val arbiter = StreamArbiterFactory.roundRobin.on(fanouts.map(_(i)))
        arbiter >> io.outputs(i)
    }
}

object ControlCommand extends SpinalEnum {
    val MODIFY_MAPPING, COMMIT_MAPPING = newElement()
}

object MappingId extends SpinalEnum {
    val InputMapper, OutputMapper, BrainEngineId, BrainState, BrainFlowState = newElement()
}

case class ControlMessage(config: EngineConfig) extends Bundle {
    val command = ControlCommand()
    val mappingId = MappingId()
    val engineId = UInt(config.engineIdWidth bits)
    val vPifoId = UInt(config.vpifoIdWidth bits)
    val flowId = UInt(config.flowIdWidth bits)
    val data = UInt(32 bits)
}

case class PifoMesh(config: EngineConfig) extends Component {
    val io = new Bundle {
        val dataRequest = master(Stream(PifoMessage(config)))
        val pop = slave(Stream(PifoMessage(config)))

        val controlRequest = slave(Stream(ControlMessage(config)))
    }

    // all datapath
    val xbar = MessageCrossBar(config)
    val pifoEngines = Seq.fill(config.numEngines)(PifoEngine(config))

    (pifoEngines zip xbar.io.outputs.tail).foreach { case (engine, out) =>
            engine.io.enqueRequest >> out
    }
    io.dataRequest >> xbar.io.inputs(0)
    xbar.io.outputs(0) >> io.pop

    // all controlpath. currently only write the memories
    val controlCommand = StreamDemux(io.controlRequest, io.controlRequest.payload.engineId, config.numEngines)
    (controlCommand zip pifoEngines).foreach { case (cmdStream, engine) =>
        engine.io.control << cmdStream
    }
}
