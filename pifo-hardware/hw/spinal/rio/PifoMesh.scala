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
    val UpdateMapperPre, UpdateMapperPost, CommitMapper,
        // brain operators
        UpdateBrainEngine, UpdateBrainState, UpdateBrainFlowState = newElement()
}

case class ControlMessage(config: EngineConfig) extends Bundle {
    val command = ControlCommand()
    val engineId = UInt(config.engineIdWidth bits)
    val vPifoId = UInt(config.vpifoIdWidth bits)
    val flowId = UInt(config.flowIdWidth bits)
    val data = UInt(config.flowStateWidth bits)
}

case class PifoMesh(config: EngineConfig) extends Component {
    val io = new Bundle {
        val dataRequest = slave(Stream(PifoMessage(config)))
        val pop = master(Stream(PifoMessage(config)))

        val insert = Vec(slave(Stream(PifoMessage(config))), config.numEngines)
        val controlRequest = slave(Stream(ControlMessage(config)))
    }

    // all datapath
    val xbar = MessageCrossBar(config)
    val pifoEngines = Seq.fill(config.numEngines)(PifoEngine(config))

    (pifoEngines zip xbar.io.outputs.tail).foreach { case (engine, out) =>
        engine.io.dequeueRequest << out
    }
    (pifoEngines zip xbar.io.inputs.tail).foreach { case (engine, in) =>
        engine.io.dequeueResponse >> in
    }

    io.dataRequest >> xbar.io.inputs(0)
    xbar.io.outputs(0) >> io.pop

    // insert path
    (io.insert zip pifoEngines).foreach { case (in, engine) =>
        engine.io.enqueRequest << in
    }

    // all controlpath. currently only write the memories
    val translatedEngineId = (io.controlRequest.payload.engineId - 1).resized
    val controlCommand = StreamDemux(io.controlRequest, translatedEngineId, config.numEngines)
    (controlCommand zip pifoEngines).foreach { case (cmdStream, engine) =>
        engine.io.control << cmdStream
    }
}
