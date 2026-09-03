package rio.sim

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

import spinal.core.sim._
import spinal.sim.SimThread

import rio._

case class RequestSimulationSettings(
    rootEngineId: Int,
    rootVPifoId: Int,
    perFlowQueueDepth: Int,
    linkBytesPerCycle: Double,
    maxCycles: Long,
    verbose: Boolean = true
) {
  require(rootEngineId > 0, s"rootEngineId must be positive, got $rootEngineId")
  require(rootVPifoId >= 0, s"rootVPifoId must be non-negative, got $rootVPifoId")
  require(perFlowQueueDepth > 0, s"perFlowQueueDepth must be positive, got $perFlowQueueDepth")
  require(
    java.lang.Double.isFinite(linkBytesPerCycle) && linkBytesPerCycle > 0.0,
    s"linkBytesPerCycle must be finite and positive, got $linkBytesPerCycle"
  )
  require(maxCycles > 0, s"maxCycles must be positive, got $maxCycles")
}

case class TreeDrainTarget(engineId: Int, vPifoId: Int) {
  require(engineId > 0, s"drain engine ID must be positive, got $engineId")
  require(vPifoId >= 0, s"drain vPifo ID must be non-negative, got $vPifoId")
}

case class ScheduledRequestActionContext(
    beforeCommit: () => Unit,
    markCommitAccepted: () => Unit,
    markCommitApplied: () => Unit,
    beginStopTheWorld: Long => (Long, Long),
    finishStopTheWorld: () => Long
)

/** One deterministic control action to run at a workload-relative cycle. */
case class ScheduledRequestAction(
    scheduledCycle: Long,
    name: String,
    drainTarget: Option[TreeDrainTarget] = None,
    mode: String = "direct",
    gatedFlowIds: Set[Int] = Set.empty,
    minimumStopCycles: Long = 0L,
    run: ScheduledRequestActionContext => Unit
) {
  require(scheduledCycle >= 0, s"scheduled action cycle must be non-negative, got $scheduledCycle")
  require(name.trim.nonEmpty, "scheduled action name must not be empty")
  require(minimumStopCycles >= 0, "minimum stop cycles must be non-negative")
  require(mode == "stop_the_world" || minimumStopCycles == 0, "minimum stop cycles require stop_the_world mode")
}

case class CompletedRequestAction(
    name: String,
    scheduledCycle: Long,
    startCycle: Long,
    commitCycle: Option[Long],
    finishCycle: Long,
    drainCycle: Option[Long] = None,
    droppedPackets: Long = 0L,
    retainedPackets: Long = 0L
) {
  require(startCycle >= scheduledCycle, "a scheduled action cannot start before its scheduled cycle")
  require(
    commitCycle.forall(cycle => cycle >= startCycle && cycle <= finishCycle),
    "an action commit must occur between its start and finish cycles"
  )
  require(finishCycle >= startCycle, "a scheduled action cannot finish before it starts")
  require(
    drainCycle.forall(_ >= startCycle),
    "an old-tree drain cannot occur before the action starts"
  )
  require(droppedPackets >= 0, "dropped packet count must be non-negative")
  require(retainedPackets >= 0, "retained packet count must be non-negative")
}

case class RequestControlInstruction(
    command: ControlCommand.E,
    engineId: Int,
    vPifoId: Int,
    flowId: Int,
    data: Int
)

case class RequestSimulationSummary(
    elapsedCycles: Long,
    submittedRequests: Long,
    admittedRequests: Long,
    completedRequests: Long,
    completedBytes: Long,
    droppedRequests: Long,
    droppedBytes: Long,
    completions: Vector[CompletedRequest],
    drops: Vector[DroppedRequest],
    completedActions: Vector[CompletedRequestAction]
)

/** Request-level harness around PifoMesh.
  *
  * The RTL schedules one token per request per engine. Simulator-side queues retain the complete request metadata and
  * are indexed by the globally unique flow ID. Root dequeues are pipelined at the
  * hardware's three-cycle per-PE initiation interval; after completion, request
  * size occupies the modeled output link for ceil(sizeBytes / linkBytesPerCycle)
  * cycles.
  */
final class PifoRequestSimulator(
    config: EngineConfig,
    dut: PifoMesh,
    controller: PifoMeshSimController,
    settings: RequestSimulationSettings,
    scheduledActions: Vector[ScheduledRequestAction] = Vector.empty
) {
  private val InsertToVisibleCycles = 8L
  private val RootTokenCapacity = config.numVPIFOs * config.fifoDepth

  require(settings.rootEngineId <= config.numEngines, s"root engine ${settings.rootEngineId} is not configured")
  require(settings.rootVPifoId < config.numVPIFOs, s"root vPifo ${settings.rootVPifoId} is not configured")
  require(
    scheduledActions.map(_.name).distinct.size == scheduledActions.size,
    "scheduled action names must be unique"
  )
  scheduledActions.flatMap(_.drainTarget).foreach { target =>
    require(target.engineId <= config.numEngines, s"drain engine ${target.engineId} is out of range")
    require(target.vPifoId < config.numVPIFOs, s"drain vPifo ${target.vPifoId} is out of range")
  }

  // The highest raw flow ID is reserved as the mesh's empty-PIFO token.
  val requestQueues = new RequestQueueBank(settings.perFlowQueueDepth, config.numVPIFOs - 2)

  private case class PendingRequest(sequence: Long, request: SimRequest)
  private implicit val pendingOrdering: Ordering[PendingRequest] = Ordering
    .by[PendingRequest, (Long, Long)](pending => (pending.request.cycle, pending.sequence))
    .reverse

  private val pending = mutable.PriorityQueue.empty[PendingRequest]
  private val rootTokenReadyCycles = mutable.Queue.empty[Long]
  private val linkPrefetchReleaseCycles = mutable.Queue.empty[Long]
  private val submittedIds = mutable.Set.empty[Long]
  private val remainingActions = mutable.Queue.from(scheduledActions.sortBy(_.scheduledCycle))
  private val completedActions = mutable.ArrayBuffer.empty[CompletedRequestAction]
  private val recentPifoPops = mutable.Queue.empty[String]
  private case class FlowGate(scheduledCycle: Long, flowIds: Set[Int], var released: Boolean)
  private val flowGates = mutable.Map.from(
    scheduledActions.map(action =>
      action.name -> FlowGate(action.scheduledCycle, action.gatedFlowIds, released = false)
    )
  )
  private case class DrainWatch(
      target: TreeDrainTarget,
      var armed: Boolean,
      var rootDrained: Boolean,
      var cycle: Option[Long]
  )
  private val drainWatches = mutable.Map.from(
    scheduledActions.flatMap { action =>
      action.drainTarget.map { target =>
        action.name -> DrainWatch(target, armed = false, rootDrained = false, cycle = None)
      }
    }
  )

  private var nextSequence = 0L
  private var currentCycle = 0L
  private var nextDequeueCycle = 0L
  private var nextRootRequestCycle = 0L
  private var lastTokenReadyCycle = 0L
  private var dequeuesInFlight = 0
  private var inputClosed = false
  private var running = false
  private var terminalFailure = Option.empty[Throwable]
  private var actionInFlight = false
  private var admissionInFlight = false
  private var commitAdmissionBarrier = false
  private var dequeuePaused = false
  private var stopWorldDeadline = Option.empty[Long]
  private var stopWorldTokens = Option.empty[Vector[Int]]
  private var cycleThread = Option.empty[SimThread]
  private var admissionThread = Option.empty[SimThread]
  private var dequeueThread = Option.empty[SimThread]
  private var actionThread = Option.empty[SimThread]

  def cycle: Long = currentCycle

  def submit(request: SimRequest): Unit = {
    require(!inputClosed, "request input is already closed")
    require(
      request.globalFlowId <= requestQueues.maxGlobalFlowId,
      s"globalFlowId ${request.globalFlowId} is outside [0, ${requestQueues.maxGlobalFlowId}]; " +
        s"flow ${config.numVPIFOs - 1} is reserved for empty-PIFO responses"
    )
    require(!submittedIds.contains(request.requestId), s"duplicate requestId ${request.requestId}")

    pending.enqueue(PendingRequest(nextSequence, request))
    nextSequence += 1
    submittedIds += request.requestId
  }

  def submitAll(requests: Iterable[SimRequest]): Unit = requests.foreach(submit)

  def closeInput(): Unit = inputClosed = true

  /** Start a live request socket. The feeder should finish with `command=end`. */
  def startRequestSocket(socketPath: String): SimThread = {
    var feederBaseCycle = 0L
    UnixDomainSocketLineServer.startKeyValue(socketPath, "RequestSocket") { line =>
      try {
        RequestSocketProtocol.parse(line) match {
          case RequestSocketProtocol.Begin => feederBaseCycle = currentCycle
          case RequestSocketProtocol.Submit(request) =>
            submit(request.copy(cycle = feederBaseCycle + request.cycle))
          case RequestSocketProtocol.End => closeInput()
        }
      } catch {
        case error: IllegalArgumentException => println(s"[RequestSocket] Ignoring message: ${error.getMessage}")
      }
    }
  }

  /** Run until closed input has drained, a worker fails, or maxCycles is reached. */
  def run(): RequestSimulationSummary = {
    require(!running, "request simulator is already running")
    running = true
    startWorkers()

    while (running && terminalFailure.isEmpty && !isFinished && currentCycle < settings.maxCycles) {
      dut.clockDomain.waitRisingEdge()
    }

    if (terminalFailure.isEmpty && !isFinished) {
      val perFlow = requestQueues.activeFlowIds
        .map(id => s"$id:${requestQueues.queuedForFlow(id)}")
        .mkString(",")
      fail(
        new IllegalStateException(
          s"request simulation timed out at cycle $currentCycle: submitted=${submittedIds.size}, " +
            s"pending=${pending.size}, queued=${requestQueues.totalQueued}, dequeuesInFlight=$dequeuesInFlight, " +
            s"linkPrefetched=${linkPrefetchReleaseCycles.size}, " +
            s"perFlow=$perFlow, " +
            s"inputClosed=$inputClosed"
        )
      )
    }
    running = false
    awaitWorkers()
    terminalFailure.foreach(error => simFailure(error.getMessage))

    val snapshot = requestQueues.snapshot
    RequestSimulationSummary(
      elapsedCycles = currentCycle,
      submittedRequests = submittedIds.size,
      admittedRequests = snapshot.admittedRequests,
      completedRequests = snapshot.completedRequests,
      completedBytes = snapshot.completedBytes,
      droppedRequests = snapshot.droppedRequests,
      droppedBytes = snapshot.droppedBytes,
      completions = requestQueues.completions,
      drops = requestQueues.drops,
      completedActions = completedActions.toVector.map { action =>
        action.copy(
          drainCycle = action.drainCycle.orElse(
            drainWatches.get(action.name).flatMap(_.cycle)
          )
        )
      }
    )
  }

  private def startWorkers(): Unit = {
    // Keep the root address stable even between request handshakes so the
    // engine's combinational popPortEmpty status is meaningful to the driver.
    dut.io.dataRequest.payload.engineId #= settings.rootEngineId
    dut.io.dataRequest.payload.vPifoId #= settings.rootVPifoId
    cycleThread = Some(fork {
      guardWorker {
        // Scheduler tokens enter a small prefetch buffer; packet bytes are
        // serialized by the request model below. Packet serialization must not
        // backpressure the PIFO's non-backpressurable pop-response Flow.
        dut.io.pop.ready #= true
        while (running) {
          dut.clockDomain.waitRisingEdge()
          if (running) {
            currentCycle += 1
            releaseLinkPrefetch()
            observePifoResponses()
            observeTreeDrains()
            if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) completePoppedRequest()
          }
        }
      }
    })

    admissionThread = Some(fork {
      guardWorker {
        while (running) {
          if (commitAdmissionBarrier) {
            dut.clockDomain.waitRisingEdge()
          } else
            dequeueAdmissibleRequest() match {
              case Some(request) =>
                // Admit metadata only after all engine tokens have been inserted. This prevents the root token from being
                // dequeued while a lower-level token for the same request is still being installed.
                admissionInFlight = true
                try {
                  controller.enque(request.globalFlowId)
                  requestQueues.enqueue(request, currentCycle)
                  val readyCycle = currentCycle + InsertToVisibleCycles
                  rootTokenReadyCycles.enqueue(readyCycle)
                  lastTokenReadyCycle = readyCycle
                } finally admissionInFlight = false
                if (settings.verbose) {
                  println(
                    s"[RequestSim] admitted cycle=$currentCycle requestId=${request.requestId} " +
                      s"flow=${request.globalFlowId} size=${request.sizeBytes}"
                  )
                }
              case None => dut.clockDomain.waitRisingEdge()
            }
        }
      }
    })

    dequeueThread = Some(fork {
      guardWorker {
        while (running) {
          if (
            !dequeuePaused &&
            currentCycle >= nextRootRequestCycle &&
            rootTokenReadyCycles.headOption.exists(_ <= currentCycle) &&
            !rootPortEmpty &&
            dequeuesInFlight + linkPrefetchReleaseCycles.size < config.prefetchBufferDepth &&
            requestQueues.totalQueued > dequeuesInFlight
          ) {
            dequeuesInFlight += 1
            rootTokenReadyCycles.dequeue()
            controller.requestDequeue(settings.rootEngineId, settings.rootVPifoId)
            // requestDequeue returns on this cycle's falling edge. Waiting two
            // more rising edges before presenting the next valid makes the
            // accepted root-pop interval exactly three cycles.
            nextRootRequestCycle = currentCycle + 2
          } else {
            dut.clockDomain.waitRisingEdge()
          }
        }
      }
    })

    if (remainingActions.nonEmpty) {
      actionThread = Some(fork {
        guardWorker {
          while (running) {
            remainingActions.headOption match {
              case Some(action) if currentCycle >= action.scheduledCycle =>
                remainingActions.dequeue()
                actionInFlight = true
                val startCycle = currentCycle
                var commitCycle = Option.empty[Long]
                var directDrainCycle = Option.empty[Long]
                var droppedPackets = 0L
                var retainedPackets = 0L
                action.run(
                  ScheduledRequestActionContext(
                    beforeCommit = () => {
                      if (!commitAdmissionBarrier) beginCommitAdmissionBarrier()
                    },
                    markCommitAccepted = () => {
                      require(commitCycle.isEmpty, s"scheduled action '${action.name}' accepted more than one commit")
                      commitCycle = Some(currentCycle)
                    },
                    markCommitApplied = () => {
                      armTreeDrain(action.name)
                      releaseFlowGate(action.name)
                      if (action.mode == "in_place") {
                        directDrainCycle = Some(currentCycle)
                      }
                      if (action.mode != "stop_the_world") endCommitAdmissionBarrier()
                    },
                    beginStopTheWorld = minimumStopCycles => {
                      require(
                        action.mode == "stop_the_world",
                        s"scheduled action '${action.name}' is not stop-the-world"
                      )
                      val result = beginStopTheWorld(minimumStopCycles)
                      directDrainCycle = Some(result._1)
                      retainedPackets = result._2
                      result
                    },
                    finishStopTheWorld = () => {
                      require(
                        action.mode == "stop_the_world",
                        s"scheduled action '${action.name}' is not stop-the-world"
                      )
                      finishStopTheWorld()
                    }
                  )
                )
                completedActions += CompletedRequestAction(
                  name = action.name,
                  scheduledCycle = action.scheduledCycle,
                  startCycle = startCycle,
                  commitCycle = commitCycle,
                  finishCycle = currentCycle,
                  drainCycle = directDrainCycle,
                  droppedPackets = droppedPackets,
                  retainedPackets = retainedPackets
                )
                actionInFlight = false
              case _ => dut.clockDomain.waitRisingEdge()
            }
          }
        }
      })
    }
  }

  private def dequeueAdmissibleRequest(): Option[SimRequest] = {
    val deferred = mutable.ArrayBuffer.empty[PendingRequest]
    var selected = Option.empty[SimRequest]

    while (selected.isEmpty && pending.headOption.exists(_.request.cycle <= currentCycle)) {
      val candidate = pending.dequeue()
      if (
        !flowBlocked(candidate.request.globalFlowId) &&
        // Metadata remains queued until the terminal PE pops. Counting it is
        // conservative for every PE, including lower-tree tokens that outlive
        // an already-issued root pop during a catch-up burst.
        requestQueues.totalQueued < RootTokenCapacity &&
        requestQueues.canEnqueue(candidate.request.globalFlowId)
      ) {
        selected = Some(candidate.request)
      } else {
        deferred += candidate
      }
    }
    deferred.foreach(candidate => pending.enqueue(candidate))
    selected
  }

  private def awaitWorkers(): Unit = {
    // Let workers blocked on a clock edge observe running=false, then join them before ending the simulation. SpinalSim
    // can strand its reusable JVM workers if the root exits while child SimThreads are still completing a barrier.
    dut.clockDomain.waitRisingEdge()
    Seq(cycleThread, admissionThread, dequeueThread, actionThread).flatten.foreach(_.join())
    cycleThread = None
    admissionThread = None
    dequeueThread = None
    actionThread = None
  }

  private def completePoppedRequest(): Unit = {
    val outputEngine = dut.io.pop.payload.engineId.toInt
    val globalFlowId = dut.io.pop.payload.vPifoId.toInt
    if (outputEngine != 0) {
      fail(
        new IllegalStateException(s"mesh output returned non-terminal engineId=$outputEngine at cycle $currentCycle")
      )
      return
    }

    val outputCycle = math.max(currentCycle, nextDequeueCycle)
    requestQueues.dequeue(globalFlowId, outputCycle) match {
      case Some(completed) =>
        val serializationCycles =
          math.max(1L, math.ceil(completed.request.sizeBytes / settings.linkBytesPerCycle).toLong)
        nextDequeueCycle = outputCycle + serializationCycles
        if (outputCycle > currentCycle) linkPrefetchReleaseCycles.enqueue(outputCycle)
        dequeuesInFlight -= 1
        if (requestQueues.isEmpty) completeArmedDrains(currentCycle)
        if (settings.verbose) {
          println(
            s"[RequestSim] completed cycle=$currentCycle requestId=${completed.request.requestId} " +
              s"flow=$globalFlowId size=${completed.request.sizeBytes} nextDequeue=$nextDequeueCycle"
          )
        }
      case None =>
        fail(
          new IllegalStateException(
            s"mesh popped flow $globalFlowId at cycle $currentCycle, but its simulator request queue is empty; " +
              "recent PIFO pops: " + recentPifoPops.mkString(" | ")
          )
        )
    }
  }

  private def isFinished: Boolean =
    inputClosed && pending.isEmpty && requestQueues.isEmpty && dequeuesInFlight == 0 &&
      rootTokenReadyCycles.isEmpty && linkPrefetchReleaseCycles.isEmpty && currentCycle >= nextDequeueCycle &&
      requestQueues.snapshot.completedRequests + requestQueues.snapshot.droppedRequests == submittedIds.size &&
      remainingActions.isEmpty && !actionInFlight &&
      drainWatches.valuesIterator.forall(watch => !watch.armed || watch.cycle.nonEmpty)

  private def beginCommitAdmissionBarrier(): Unit = {
    require(!commitAdmissionBarrier, "a commit admission barrier is already active")
    commitAdmissionBarrier = true
    while (admissionInFlight) dut.clockDomain.waitRisingEdge()
    while (currentCycle < lastTokenReadyCycle) dut.clockDomain.waitRisingEdge()
  }

  private def beginStopTheWorld(minimumStopCycles: Long): (Long, Long) = {
    require(!commitAdmissionBarrier, "an admission barrier is already active")
    require(!dequeuePaused, "root dequeue is already paused")
    require(stopWorldDeadline.isEmpty && stopWorldTokens.isEmpty, "a stop-the-world action is already active")
    require(minimumStopCycles >= 0, "minimum stop cycles must be non-negative")
    commitAdmissionBarrier = true
    dequeuePaused = true
    while (
      admissionInFlight || currentCycle < lastTokenReadyCycle || dequeuesInFlight > 0 ||
      linkPrefetchReleaseCycles.nonEmpty || currentCycle < nextDequeueCycle
    ) {
      dut.clockDomain.waitRisingEdge()
    }
    val drainCycle = currentCycle
    val retained = requestQueues.queuedRequestsInAdmissionOrder.map(_.request.globalFlowId)
    require(
      retained.size <= RootTokenCapacity,
      s"cannot replay ${retained.size} root tokens into capacity $RootTokenCapacity"
    )
    rootTokenReadyCycles.clear()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge(2)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitRisingEdge()
    stopWorldDeadline = Some(drainCycle + minimumStopCycles)
    stopWorldTokens = Some(retained)
    (drainCycle, retained.size.toLong)
  }

  private def finishStopTheWorld(): Long = {
    val deadline = stopWorldDeadline.getOrElse(
      throw new IllegalStateException("no stop-the-world action is active")
    )
    val tokens = stopWorldTokens.getOrElse(
      throw new IllegalStateException("no stop-the-world token snapshot is available")
    )
    tokens.foreach { flowId =>
      controller.enque(flowId)
      val readyCycle = currentCycle + InsertToVisibleCycles
      rootTokenReadyCycles.enqueue(readyCycle)
      lastTokenReadyCycle = readyCycle
    }
    val resumeCycle = math.max(deadline, lastTokenReadyCycle)
    while (currentCycle < resumeCycle) dut.clockDomain.waitRisingEdge()
    stopWorldDeadline = None
    stopWorldTokens = None
    endCommitAdmissionBarrier()
    currentCycle
  }

  private def endCommitAdmissionBarrier(): Unit = {
    require(commitAdmissionBarrier, "no commit admission barrier is active")
    commitAdmissionBarrier = false
    dequeuePaused = false
  }

  private def flowBlocked(flowId: Int): Boolean =
    flowGates.valuesIterator.exists(gate =>
      !gate.released &&
        currentCycle >= gate.scheduledCycle &&
        gate.flowIds.contains(flowId)
    )

  private def releaseLinkPrefetch(): Unit = {
    while (linkPrefetchReleaseCycles.headOption.exists(_ <= currentCycle)) {
      linkPrefetchReleaseCycles.dequeue()
    }
  }

  private def rootPortEmpty: Boolean =
    dut.pifoEngines(settings.rootEngineId - 1).pifos.io.popPortEmpty.toBoolean

  private def observePifoResponses(): Unit = {
    dut.pifoEngines.zipWithIndex.foreach { case (engine, index) =>
      val response = engine.pifos.io.popResponse
      if (response.valid.toBoolean) {
        if (!response.exist.toBoolean) {
          fail(
            new IllegalStateException(
              s"PIFO underflow at cycle $currentCycle engine=${index + 1} port=${response.port.toInt}; " +
                s"queued=${requestQueues.totalQueued} rootReady=${rootTokenReadyCycles.size} " +
                s"dequeuesInFlight=$dequeuesInFlight linkPrefetched=${linkPrefetchReleaseCycles.size}"
            )
          )
        } else {
          recentPifoPops.enqueue(
            s"cycle=$currentCycle engine=${index + 1} port=${response.port.toInt} " +
              s"data=${response.data.toInt} priority=${response.priority.toBigInt}"
          )
          while (recentPifoPops.size > 24) recentPifoPops.dequeue()
        }
      }
    }
  }

  private def releaseFlowGate(actionName: String): Unit = {
    flowGates.get(actionName).foreach(_.released = true)
  }

  private def armTreeDrain(actionName: String): Unit = {
    drainWatches.get(actionName).foreach { watch =>
      watch.armed = true
      if (requestQueues.isEmpty && dequeuesInFlight == 0) watch.cycle = Some(currentCycle)
    }
  }

  private def observeTreeDrains(): Unit = {
    drainWatches.valuesIterator.filter(watch => watch.armed && watch.cycle.isEmpty).foreach { watch =>
      val drained = dut.pifoEngines(watch.target.engineId - 1).pifos.io.portDrained
      if (
        drained.valid.toBoolean &&
        drained.payload.toInt == watch.target.vPifoId
      ) {
        watch.rootDrained = true
        watch.cycle = Some(currentCycle)
      }
    }
  }

  private def completeArmedDrains(cycle: Long): Unit = {
    drainWatches.valuesIterator
      .filter(watch => watch.armed && watch.cycle.isEmpty)
      .foreach(_.cycle = Some(cycle))
  }

  private def guardWorker(body: => Unit): Unit = {
    try body
    catch {
      case NonFatal(error) => fail(error)
    }
  }

  private def fail(error: Throwable): Unit = {
    if (terminalFailure.isEmpty) terminalFailure = Some(error)
  }
}

object RequestSocketProtocol {
  sealed trait Message
  case class Submit(request: SimRequest) extends Message
  case object Begin extends Message
  case object End extends Message

  private val requestFields = Set("command", "cycle", "requestId", "globalFlowId", "sizeBytes", "size")

  def parse(line: UnixDomainSocketLineServer.KeyValueLine): Message = {
    line.fields.get("command") match {
      case Some("begin") =>
        require(line.fields.keySet == Set("command"), "command=begin accepts no other fields")
        Begin
      case Some("end") =>
        require(line.fields.keySet == Set("command"), "command=end accepts no other fields")
        End
      case Some(command) if command != "request" =>
        throw new IllegalArgumentException(s"unknown request command '$command'")
      case _ =>
        val unknown = line.fields.keySet.diff(requestFields)
        require(unknown.isEmpty, s"unknown fields: ${unknown.toSeq.sorted.mkString(", ")}")
        val sizeValues = Seq("sizeBytes", "size").flatMap(line.fields.get)
        require(sizeValues.size == 1, "exactly one of sizeBytes or size is required")
        Submit(
          SimRequest(
            cycle = decodeLong(line.requireString("cycle")),
            requestId = decodeLong(line.requireString("requestId")),
            globalFlowId = line.requireInt("globalFlowId"),
            sizeBytes = decodeInt(sizeValues.head)
          )
        )
    }
  }

  private def decodeLong(value: String): Long = {
    try java.lang.Long.decode(value).longValue()
    catch {
      case _: NumberFormatException => throw new IllegalArgumentException(s"invalid integer '$value'")
    }
  }

  private def decodeInt(value: String): Int = {
    val decoded = decodeLong(value)
    require(decoded >= Int.MinValue && decoded <= Int.MaxValue, s"integer '$value' does not fit in 32 bits")
    decoded.toInt
  }
}

/** File and demo configuration helpers for RequestSimulatorCli. */
object RequestSimulationConfiguration {
  private val commandByName = Map(
    "UpdateMapperPre" -> ControlCommand.UpdateMapperPre,
    "UpdateMapperPost" -> ControlCommand.UpdateMapperPost,
    "UpdateMapperNonExist" -> ControlCommand.UpdateMapperNonExist,
    "CommitMapper" -> ControlCommand.CommitMapper,
    "UpdateBrainEngine" -> ControlCommand.UpdateBrainEngine,
    "UpdateBrainState" -> ControlCommand.UpdateBrainState,
    "UpdateBrainFlowState" -> ControlCommand.UpdateBrainFlowState
  )
  private val commandFields = Set("command", "engineId", "vPifoId", "flowId", "data")

  private[sim] def parseControlInstruction(
      fields: Map[String, String],
      location: String
  ): RequestControlInstruction = {
    val unknown = fields.keySet.diff(commandFields)
    require(unknown.isEmpty, s"$location: unknown fields: ${unknown.toSeq.sorted.mkString(", ")}")
    val missing = commandFields.diff(fields.keySet)
    require(missing.isEmpty, s"$location: missing fields: ${missing.toSeq.sorted.mkString(", ")}")
    val commandName = fields("command")
    val command = commandByName.getOrElse(
      commandName,
      throw new IllegalArgumentException(s"$location: unknown command '$commandName'")
    )
    try {
      RequestControlInstruction(
        command = command,
        engineId = UnixDomainSocketLineServer.parseInt(fields("engineId")),
        vPifoId = UnixDomainSocketLineServer.parseInt(fields("vPifoId")),
        flowId = UnixDomainSocketLineServer.parseInt(fields("flowId")),
        data = UnixDomainSocketLineServer.parseInt(fields("data"))
      )
    } catch {
      case error: NumberFormatException =>
        throw new IllegalArgumentException(s"$location: invalid integer: ${error.getMessage}", error)
    }
  }

  def loadControlFile(path: Path, controller: PifoMeshSimController): Unit = {
    loadControlInstructions(path).foreach(instruction => sendInstruction(instruction, controller))
  }

  def loadControlInstructions(path: Path): Vector[RequestControlInstruction] = {
    Files
      .readAllLines(path, StandardCharsets.UTF_8)
      .asScala
      .zipWithIndex
      .flatMap { case (raw, index) =>
        try {
          UnixDomainSocketLineServer.parseKeyValueLine(raw).map { line =>
            parseControlInstruction(line.fields, s"$path:${index + 1}")
          }
        } catch {
          case error: IllegalArgumentException =>
            if (error.getMessage.startsWith(s"$path:${index + 1}:")) throw error
            throw new IllegalArgumentException(s"$path:${index + 1}: ${error.getMessage}", error)
        }
      }
      .toVector
  }

  def executeTransactionPackage(
      instructions: Vector[RequestControlInstruction],
      controller: PifoMeshSimController,
      beforeCommit: () => Unit,
      markCommitAccepted: () => Unit,
      onCommitApplied: () => Unit
  ): Unit = {
    validateTransactionPackage(instructions)
    instructions.foreach { instruction =>
      val isCommit = instruction.command == ControlCommand.CommitMapper
      if (isCommit) beforeCommit()
      sendInstruction(
        instruction,
        controller,
        onCommitApplied = if (isCommit) onCommitApplied else () => (),
        onAccepted = if (isCommit) markCommitAccepted else () => ()
      )
    }
  }

  def validateTransactionPackage(instructions: Vector[RequestControlInstruction]): Unit = {
    val commitIndexes = instructions.zipWithIndex.collect {
      case (instruction, index) if instruction.command == ControlCommand.CommitMapper => index
    }
    require(
      commitIndexes == Vector(instructions.size - 1),
      "transaction package must end with exactly one CommitMapper"
    )
  }

  private def sendInstruction(
      instruction: RequestControlInstruction,
      controller: PifoMeshSimController,
      onCommitApplied: () => Unit = () => (),
      onAccepted: () => Unit = () => ()
  ): Unit = {
    controller.sendControl(
      instruction.command,
      instruction.engineId,
      instruction.data,
      vPifoId = instruction.vPifoId,
      flowId = instruction.flowId,
      onCommitApplied = onCommitApplied,
      onAccepted = onAccepted
    )
  }

  def configureFlatFifo(
      config: EngineConfig,
      controller: PifoMeshSimController,
      rootEngineId: Int,
      rootVPifoId: Int,
      globalFlowIds: Iterable[Int]
  ): Unit = {
    require(rootEngineId >= 1 && rootEngineId <= config.numEngines, s"invalid root engine $rootEngineId")
    require(rootVPifoId >= 0 && rootVPifoId < config.numVPIFOs, s"invalid root vPifo $rootVPifoId")

    // BrainType encodings are NOP=0, WFQ=1, SP=2, FIFO=3.
    controller.sendControl(ControlCommand.UpdateBrainEngine, rootEngineId, 3, vPifoId = rootVPifoId)
    globalFlowIds.toSeq.distinct.sorted.foreach { globalFlowId =>
      require(
        globalFlowId >= 0 && globalFlowId < config.numVPIFOs - 1,
        s"global flow $globalFlowId collides with the reserved empty-flow token or exceeds the configured width"
      )
      controller.sendControl(ControlCommand.UpdateMapperPre, rootEngineId, rootVPifoId, vPifoId = globalFlowId)
      controller.sendControl(
        ControlCommand.UpdateMapperPost,
        rootEngineId,
        controller.mkFlowId(0, globalFlowId),
        vPifoId = rootVPifoId,
        flowId = controller.mkFlowId(rootEngineId, globalFlowId)
      )
    }
    controller.sendControl(ControlCommand.CommitMapper, engineId = 1, data = 0)
  }
}
