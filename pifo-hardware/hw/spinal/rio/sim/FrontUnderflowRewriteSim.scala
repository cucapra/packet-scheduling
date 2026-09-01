package rio.sim

import scala.collection.mutable.ArrayBuffer

import spinal.core.sim._

import rio._

/** Cycle-level checks for the per-engine front underflow rewrite.
  *
  * Run from pifo-hardware with:
  *   sbt "runMain rio.sim.FrontUnderflowRewriteSim"
  */
object FrontUnderflowRewriteSim extends App {
  private val clockPeriod = 10L
  private val config = EngineConfig(
    numEngines = 1,
    numVPIFOs = 8,
    maxPacketPriority = 64,
    fifoDepth = 8,
    prefetchBufferDepth = 2
  )

  private val sourcePifo = 1
  private val targetPifo = 2
  private val initiallyEmptySourcePifo = 3
  private val initiallyEmptyTargetPifo = 4
  private val rawFlow = 1
  private val engineFlow = (1 << config.vpifoIdWidth) | rawFlow

  SimConfig.withIVerilog
    .addSimulatorFlag("-g2012")
    .compile {
      val engine = PifoEngine(config)
      engine.pifos.io.popRequest.valid.simPublic()
      engine.pifos.io.popRequest.port.simPublic()
      engine.pifos.io.popResponse.valid.simPublic()
      engine.pifos.io.popResponse.exist.simPublic()
      engine.deque.frontRewrite.enabled(sourcePifo).simPublic()
      engine
    }
    .doSim { dut =>
      dut.clockDomain.forkStimulus(clockPeriod)

      dut.io.enqueRequest.valid #= false
      dut.io.enqueRequest.payload.engineId #= 0
      dut.io.enqueRequest.payload.vPifoId #= 0
      dut.io.dequeueRequest.valid #= false
      dut.io.dequeueRequest.payload.engineId #= 0
      dut.io.dequeueRequest.payload.vPifoId #= 0
      dut.io.dequeueResponse.ready #= true
      dut.io.control.valid #= false
      dut.io.control.payload.command #= ControlCommand.UpdateMapperPre
      dut.io.control.payload.engineId #= 0
      dut.io.control.payload.vPifoId #= 0
      dut.io.control.payload.flowId #= 0
      dut.io.control.payload.data #= 0

      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(4)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(4)

      val popRequests = ArrayBuffer.empty[(Long, Int)]
      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          if (dut.pifos.io.popRequest.valid.toBoolean) {
            popRequests += simTime() -> dut.pifos.io.popRequest.port.toInt
          }
        }
      }

      def sendControl(
          command: ControlCommand.E,
          vPifoId: Int = 0,
          flowId: Int = 0,
          data: Int = 0
      ): Unit = {
        dut.io.control.valid #= true
        dut.io.control.payload.command #= command
        dut.io.control.payload.engineId #= 1
        dut.io.control.payload.vPifoId #= vPifoId
        dut.io.control.payload.flowId #= flowId
        dut.io.control.payload.data #= data
        dut.clockDomain.waitSamplingWhere(dut.io.control.ready.toBoolean)
        dut.io.control.valid #= false
      }

      def enqueue(): Unit = {
        dut.io.enqueRequest.valid #= true
        dut.io.enqueRequest.payload.engineId #= 1
        dut.io.enqueRequest.payload.vPifoId #= rawFlow
        dut.clockDomain.waitSamplingWhere(dut.io.enqueRequest.ready.toBoolean)
        dut.io.enqueRequest.valid #= false
      }

      def dequeue(vPifoId: Int = sourcePifo): Unit = {
        dut.io.dequeueRequest.valid #= true
        dut.io.dequeueRequest.payload.engineId #= 1
        dut.io.dequeueRequest.payload.vPifoId #= vPifoId
        dut.clockDomain.waitSamplingWhere(dut.io.dequeueRequest.ready.toBoolean)
        dut.io.dequeueRequest.valid #= false
      }

      // Publish an initial tree whose physical root is sourcePifo.
      sendControl(ControlCommand.UpdateBrainEngine, vPifoId = sourcePifo, data = 3)
      sendControl(ControlCommand.UpdateMapperPre, vPifoId = rawFlow, data = sourcePifo)
      sendControl(
        ControlCommand.UpdateMapperPost,
        vPifoId = sourcePifo,
        flowId = engineFlow,
        data = rawFlow
      )
      sendControl(ControlCommand.CommitMapper)
      dut.clockDomain.waitSampling(2)

      // Install the next transition in the single-bank table. It remains
      // disabled and unarmed until the following commit.
      sendControl(
        ControlCommand.UpdateMapperNonExist,
        vPifoId = sourcePifo,
        data = targetPifo
      )

      // A staged entry is inert: a pre-commit miss does not enable it.
      popRequests.clear()
      dequeue()
      dut.clockDomain.waitSampling(4)
      assert(popRequests.map(_._2) == Seq(sourcePifo), s"unexpected pre-commit rewrite: $popRequests")
      assert(!dut.deque.frontRewrite.enabled(sourcePifo).toBoolean)

      // These packets belong to the old tree. Only the second source pop may
      // activate the rewrite.
      enqueue()
      enqueue()
      dut.clockDomain.waitSampling(8)

      sendControl(ControlCommand.UpdateBrainEngine, vPifoId = targetPifo, data = 3)
      sendControl(ControlCommand.UpdateMapperPre, vPifoId = rawFlow, data = targetPifo)
      sendControl(
        ControlCommand.UpdateMapperPost,
        vPifoId = targetPifo,
        flowId = engineFlow,
        data = rawFlow
      )
      sendControl(ControlCommand.CommitMapper)
      dut.clockDomain.waitSampling(2)

      // This packet belongs to the new tree.
      enqueue()
      dut.clockDomain.waitSampling(8)

      // Three back-to-back dequeue intents pop both source entries and then the
      // target. The first source pop keeps the rewrite disabled. The drain pulse
      // from the second holds the target request for one cycle so it observes
      // the registered enable; there is no retry or underflowing source lookup.
      popRequests.clear()
      val outputObserver = fork {
        (0 until 3).foreach { _ =>
          dut.clockDomain.waitSamplingWhere(dut.io.dequeueResponse.valid.toBoolean)
          assert(dut.io.dequeueResponse.payload.engineId.toInt == 0)
          assert(dut.io.dequeueResponse.payload.vPifoId.toInt == rawFlow)
        }
      }
      dequeue()
      dequeue()
      dequeue()
      outputObserver.join()
      assert(
        popRequests.map(_._2) == Seq(sourcePifo, sourcePifo, targetPifo),
        s"unexpected boundary pops: $popRequests"
      )
      assert(
        popRequests(1)._1 - popRequests(0)._1 == clockPeriod,
        s"rewrite enabled before the final source pop: $popRequests"
      )
      val boundaryGap = popRequests(2)._1 - popRequests(1)._1
      assert(
        boundaryGap >= clockPeriod && boundaryGap <= 3 * clockPeriod,
        s"boundary II exceeded three cycles: gap=$boundaryGap"
      )
      assert(dut.deque.frontRewrite.enabled(sourcePifo).toBoolean)

      // The enabled table sustains one accepted request per engine cycle.
      (0 until 3).foreach { _ => enqueue() }
      dut.clockDomain.waitSampling(8)
      popRequests.clear()
      (0 until 3).foreach { _ => dequeue() }
      dut.clockDomain.waitSampling(4)
      val targetRequests = popRequests.filter(_._2 == targetPifo)
      assert(targetRequests.size == 3, s"expected three rewritten pops, got $popRequests")
      val gaps = targetRequests.sliding(2).map(pair => pair(1)._1 - pair(0)._1).toVector
      assert(gaps.forall(_ == clockPeriod), s"front rewrite II is not one cycle: gaps=$gaps")

      // If a source was already empty when its entry became armed, its first
      // request is held for the enable cycle and then directly targets the new
      // PIFO. It never performs an underflowing source pop.
      sendControl(ControlCommand.UpdateBrainEngine, vPifoId = initiallyEmptyTargetPifo, data = 3)
      sendControl(
        ControlCommand.UpdateMapperNonExist,
        vPifoId = initiallyEmptySourcePifo,
        data = initiallyEmptyTargetPifo
      )
      sendControl(ControlCommand.UpdateMapperPre, vPifoId = rawFlow, data = initiallyEmptyTargetPifo)
      sendControl(
        ControlCommand.UpdateMapperPost,
        vPifoId = initiallyEmptyTargetPifo,
        flowId = engineFlow,
        data = rawFlow
      )
      sendControl(ControlCommand.CommitMapper)
      dut.clockDomain.waitSampling(2)
      enqueue()
      dut.clockDomain.waitSampling(8)
      popRequests.clear()
      dequeue(initiallyEmptySourcePifo)
      dut.clockDomain.waitSampling(4)
      assert(
        popRequests.map(_._2) == Seq(initiallyEmptyTargetPifo),
        s"already-empty source was probed instead of enabled at the front: $popRequests"
      )

      simSuccess()
    }
}
