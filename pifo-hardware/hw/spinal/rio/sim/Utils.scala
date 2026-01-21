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

// TODO(zhiyuang): check the pifos and flow ids are valid
object RioPredefinedPifos {
  // pifos are 0xA to 0xF
  val rPifo : Seq[Int] = 10 until 16
  // flows are 1 to 9
  val rFlow : Seq[Int] = 1 until 10
}

case class PifoMeshSimController(
  config: EngineConfig,
  dut: PifoMesh
) {

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

  // high-level configuration interface
  case class Configer(transactional: Boolean = false) {
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

    case class TreeConfiger(tree : TreeController) {
      def addPifo(engineId : Int, pifoId : Int) : TreeConfiger = {
        assert(engineId <= config.numEngines, s"Invalid engineId: $engineId")
        assert(!tree.pifoMap.contains(pifoId), s"pifoId $pifoId already exists in the tree")
        tree.pifoMap(pifoId) = engineId
        this
      }

      def addFlow(flowId : Int, vPifos : Seq[Int]) : TreeConfiger = {
        for (i <- 0 until vPifos.length) {
          val engine = tree.pifoMap(vPifos(i))
          val vPifo = vPifos(i)

          sendControl(ControlCommand.UpdateMapperPre, engine, vPifo, vPifoId = flowId)
          val nextvPifo = if (i + 1 < vPifos.length) vPifos(i + 1) else flowId
          val nextEngine = if (i + 1 < vPifos.length) tree.pifoMap(nextvPifo) else 0
          sendControl(
            ControlCommand.UpdateMapperPost,
            engine, mkFlowId(nextEngine, nextvPifo),
            flowId = mkFlowId(engine, flowId)
          )
        }
        this
      }
      // apply the non-exist rewrite at the root pifo
      def rootNonExistRewrite(newEngineId : Int, newPifoId : Int) : TreeConfiger = {
        sendControl(
          ControlCommand.UpdateMapperPost,
          tree.rootEngine,
          mkFlowId(newEngineId, newPifoId),
          flowId = mkFlowId(tree.rootEngine, tree.rootPifo)
        )
        this
      }

      // brain operations
      // TODO(zhiyuang): limitations on transactional updates
      def brainFIFO(vPifoId: Int) : TreeConfiger = {
        // assert(!transactional, "setBrainFIFO is not supported in transactional mode")
        val engineId = tree.pifoMap(vPifoId)
        setBrainFIFO(engineId, vPifoId)
        this
      }
      def brainSP(vPifoId: Int) : TreeConfiger = {
        val engineId = tree.pifoMap(vPifoId)
        setBrainSP(engineId, vPifoId)
        this
      }
      def brainWFQ(vPifoId: Int) : TreeConfiger = {
        val engineId = tree.pifoMap(vPifoId)
        setBrainWFQ(engineId, vPifoId)
        this
      }
      def brainState(vPifoId: Int, flowId: Int, state: Int) : TreeConfiger = {
        val engineId = tree.pifoMap(vPifoId)
        setBrainState(engineId, vPifoId, flowId, state)
        this
      }
    }

    def tree(tree : TreeController) : TreeConfiger = TreeConfiger(tree)
  }

  // Transactional configuration process.
  // TODO(zhiyuang): make this async and transactional
  def transaction(F : Configer => Unit) : SimThread = {
    assert(false,  "Transactional configuration is not yet implemented")
    val configer = Configer(transactional = true)
    fork { F(configer) }
  }

  def config(F : Configer => Unit) : SimThread = {
    val configer = Configer(transactional = false)
    fork { F(configer) }
  }

  ///////////////////////////////////
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