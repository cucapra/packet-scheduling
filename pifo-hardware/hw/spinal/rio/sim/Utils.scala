package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.sim._

import rio._

object SimUtils {
    def RioSimConfig = SimConfig.withIVerilog
      .addSimulatorFlag("-g2012")
      .withFstWave
}

case class PifoMeshSimController(
  config: EngineConfig,
  dut: PifoMesh
) {
  // high-level functions
  def setBrainFIFO(engineId: Int, vPifoId: Int) = {
    sendControl(ControlCommand.UpdateBrainEngine, engineId, 3, vPifoId = vPifoId)  // FIFO
  }
  def setBrainSP(engineId: Int, vPifoId: Int) = {
    sendControl(ControlCommand.UpdateBrainEngine, engineId, 2, vPifoId = vPifoId)  // SP
  }
  def setBrainWFQ(engineId: Int, vPifoId: Int) = {
    sendControl(ControlCommand.UpdateBrainEngine, engineId, 1, vPifoId = vPifoId)  // WFQ
  }

  def setBrainState(engineId: Int, vPifoId: Int, flowId: Int, state: Int) = {
    sendControl(ControlCommand.UpdateBrainFlowState, engineId, state, vPifoId = vPifoId, flowId = mkFlowId(engineId, flowId))
  }

  // compound functions
  def enque(vPifoId: Int) = {
    for (i <- 0 until config.numEngines) {
      enqueueToEngine(i + 1, vPifoId)
    }
  }

  // primitive functions
  def mkFlowId(engineId: Int, vPifoId: Int): Int = {
    assert(engineId >= 0 && engineId <= config.numEngines, s"Invalid engineId: $engineId")
    assert(vPifoId >= 0 && vPifoId < config.numVPIFOs, s"Invalid vPifoId: $vPifoId")

    (engineId << config.vpifoIdWidth) | vPifoId
  }

  def sendControl(cmd: ControlCommand.E, engineId: Int, data: Int, vPifoId: Int = 0, flowId: Int = 0) = {
    dut.io.controlRequest.valid #= true
    dut.io.controlRequest.payload.command #= cmd
    dut.io.controlRequest.payload.engineId #= engineId
    dut.io.controlRequest.payload.vPifoId #= vPifoId
    dut.io.controlRequest.payload.flowId #= flowId
    dut.io.controlRequest.payload.data #= data
    dut.clockDomain.waitRisingEdge()
    dut.io.controlRequest.valid #= false
  }

  def enqueueToEngine(engineId: Int, vPifoId: Int) = {
    val pid = engineId - 1 // Adjust for control port offset
    dut.io.insert(pid).valid #= true
    dut.io.insert(pid).payload.engineId #= engineId
    dut.io.insert(pid).payload.vPifoId #= vPifoId
    dut.clockDomain.waitRisingEdge()
    dut.io.insert(pid).valid #= false
  }

  def requestDequeue(engineId: Int, vPifoId: Int) = {
    dut.io.dataRequest.valid #= true
    dut.io.dataRequest.payload.engineId #= engineId
    dut.io.dataRequest.payload.vPifoId #= vPifoId
    dut.clockDomain.waitRisingEdge()
    dut.io.dataRequest.valid #= false
  }

  def start = {
    // Clock generation
    fork {
      while (true) {
        sleep(5)
        dut.clockDomain.clockToggle()
        sleep(5)
        dut.clockDomain.clockToggle()
      }
    }

    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge(4)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitRisingEdge(4)

    fork {
      while (true) {
        dut.clockDomain.waitRisingEdge()
        if(dut.io.pop.valid.toBoolean) {
          val eng = dut.io.pop.payload.engineId.toLong
          val vp = dut.io.pop.payload.vPifoId.toLong
          println(s"[Monitor] Pop response: vPifoId=0x${vp.toHexString}")
        }
      }
    }
  }

  // Initialize all inputs
  for (i <- 0 until config.numEngines) {
    dut.io.insert(i).valid #= false
    dut.io.insert(i).payload.engineId #= 0
    dut.io.insert(i).payload.vPifoId #= 0
  }

  // Initialize dataRequest port
  dut.io.dataRequest.valid #= false
  dut.io.dataRequest.payload.engineId #= 0
  dut.io.dataRequest.payload.vPifoId #= 0

  // Initialize pop port
  dut.io.pop.ready #= true

  // Initialize control port
  dut.io.controlRequest.valid #= false
  dut.io.controlRequest.payload.command #= ControlCommand.UpdateMapperPre
  dut.io.controlRequest.payload.engineId #= 0
  dut.io.controlRequest.payload.vPifoId #= 0
  dut.io.controlRequest.payload.flowId #= 0
  dut.io.controlRequest.payload.data #= 0
}

// In the following world, we require the flowId to be globally unique.
// vpifo format: (engineId, vPifoId)
case class TreeController(
  meshController : PifoMeshSimController,
  pifos : Seq[(Int, Int)] = Seq(),
) {
  assert(pifos.nonEmpty, "TreeController requires at least one (engineId, pifoId) pair")
  val rootEngine : Int = pifos.head._1
  val rootPifo : Int = pifos.head._2

  val pifoMap : scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map()

  def deque = {
    meshController.requestDequeue(rootEngine, rootPifo)
  }

  // configuration interface
  case class TreeConfiger(transactional : Boolean = false) {
    def addPifo(engineId : Int, pifoId : Int) = {
      assert(engineId <= meshController.config.numEngines, s"Invalid engineId: $engineId")
      assert(!pifoMap.contains(pifoId), s"pifoId $pifoId already exists in the tree")
      pifoMap(pifoId) = engineId
    }

    def addFlow(flowId : Int, vPifos : Seq[Int]) = {
      for (i <- 0 until vPifos.length) {
        val engine = pifoMap(vPifos(i))
        val vPifo = vPifos(i)

        meshController.sendControl(ControlCommand.UpdateMapperPre, engine, vPifo, vPifoId = flowId)
        val nextvPifo = if (i + 1 < vPifos.length) vPifos(i + 1) else flowId
        val nextEngine = if (i + 1 < vPifos.length) pifoMap(nextvPifo) else 0
        meshController.sendControl(
          ControlCommand.UpdateMapperPost,
          engine, meshController.mkFlowId(nextEngine, nextvPifo),
          flowId = meshController.mkFlowId(engine, flowId)
        )
      }
    }
    // apply the non-exist rewrite at the root pifo
    def rootNonExistRewrite(newEngineId : Int, newPifoId : Int) = {
      meshController.sendControl(
        ControlCommand.UpdateMapperPost,
        rootEngine,
        meshController.mkFlowId(newEngineId, newPifoId),
        flowId = meshController.mkFlowId(rootEngine, rootPifo)
      )
    }

    // brain operations
    // TODO(zhiyuang): limitations on transactional updates
    def setBrainFIFO(vPifoId: Int) = {
      // assert(!transactional, "setBrainFIFO is not supported in transactional mode")
      val engineId = pifoMap(vPifoId)
      meshController.setBrainFIFO(engineId, vPifoId)
    }
    def setBrainSP(vPifoId: Int) = {
      val engineId = pifoMap(vPifoId)
      meshController.setBrainSP(engineId, vPifoId)
    }
    def setBrainWFQ(vPifoId: Int) = {
      val engineId = pifoMap(vPifoId)
      meshController.setBrainWFQ(engineId, vPifoId)
    }
    def setBrainState(vPifoId: Int, flowId: Int, state: Int) = {
      val engineId = pifoMap(vPifoId)
      meshController.setBrainState(engineId, vPifoId, flowId, state)
    }
  }

  // Transactional configuration process.
  // TODO(zhiyuang): make this async and transactional
  def transaction(F : TreeConfiger => Unit) : SimThread = {
    val configer = TreeConfiger(true)
    fork { F(configer) }
  }

  def async_config(F : TreeConfiger => Unit) : SimThread = {
    val configer = TreeConfiger(false)
    fork { F(configer) }
  }
  def config(F : TreeConfiger => Unit) : Unit = {
    async_config(F).join()
  }

  // init code
  pifos.foreach { case (engineId, pifoId) =>
    // insert the pifo into the map
    pifoMap(pifoId) = engineId
  }

  // make sure rootPifo forwards to output for non-exist
  meshController.sendControl(
    ControlCommand.UpdateMapperNonExist,
    rootEngine,
    meshController.mkFlowId(0, meshController.config.numVPIFOs-1),
    vPifoId = rootPifo)
}