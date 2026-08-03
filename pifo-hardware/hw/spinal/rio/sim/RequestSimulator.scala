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

case class RequestSimulationSummary(
    elapsedCycles: Long,
    submittedRequests: Long,
    admittedRequests: Long,
    completedRequests: Long,
    completedBytes: Long,
    completions: Vector[CompletedRequest]
)

/** Request-level harness around PifoMesh.
  *
  * The RTL schedules one token per request per engine. Simulator-side queues retain the complete request metadata and
  * are indexed by the globally unique flow ID. Only one root dequeue is in flight at a time; after completion, request
  * size occupies the modeled output link for ceil(sizeBytes / linkBytesPerCycle) cycles.
  */
final class PifoRequestSimulator(
    config: EngineConfig,
    dut: PifoMesh,
    controller: PifoMeshSimController,
    settings: RequestSimulationSettings
) {
  require(settings.rootEngineId <= config.numEngines, s"root engine ${settings.rootEngineId} is not configured")
  require(settings.rootVPifoId < config.numVPIFOs, s"root vPifo ${settings.rootVPifoId} is not configured")

  // The highest raw flow ID is reserved as the mesh's empty-PIFO token.
  val requestQueues = new RequestQueueBank(settings.perFlowQueueDepth, config.numVPIFOs - 2)

  private case class PendingRequest(sequence: Long, request: SimRequest)
  private implicit val pendingOrdering: Ordering[PendingRequest] = Ordering
    .by[PendingRequest, (Long, Long)](pending => (pending.request.cycle, pending.sequence))
    .reverse

  private val pending = mutable.PriorityQueue.empty[PendingRequest]
  private val submittedIds = mutable.Set.empty[Long]

  private var nextSequence = 0L
  private var currentCycle = 0L
  private var nextDequeueCycle = 0L
  private var dequeueInFlight = false
  private var inputClosed = false
  private var running = false
  private var terminalFailure = Option.empty[Throwable]
  private var cycleThread = Option.empty[SimThread]
  private var admissionThread = Option.empty[SimThread]
  private var dequeueThread = Option.empty[SimThread]

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
      fail(
        new IllegalStateException(
          s"request simulation timed out at cycle $currentCycle: submitted=${submittedIds.size}, " +
            s"pending=${pending.size}, queued=${requestQueues.totalQueued}, dequeueInFlight=$dequeueInFlight, " +
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
      completions = requestQueues.completions
    )
  }

  private def startWorkers(): Unit = {
    cycleThread = Some(fork {
      guardWorker {
        while (running) {
          dut.clockDomain.waitRisingEdge()
          if (running) {
            currentCycle += 1
            if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) completePoppedRequest()
          }
        }
      }
    })

    admissionThread = Some(fork {
      guardWorker {
        while (running) {
          dequeueAdmissibleRequest() match {
            case Some(request) =>
              // Admit metadata only after all engine tokens have been inserted. This prevents the root token from being
              // dequeued while a lower-level token for the same request is still being installed.
              controller.enque(request.globalFlowId)
              requestQueues.enqueue(request, currentCycle)
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
          if (!dequeueInFlight && requestQueues.totalQueued > 0 && currentCycle >= nextDequeueCycle) {
            dequeueInFlight = true
            controller.requestDequeue(settings.rootEngineId, settings.rootVPifoId)
          } else {
            dut.clockDomain.waitRisingEdge()
          }
        }
      }
    })
  }

  private def dequeueAdmissibleRequest(): Option[SimRequest] = {
    val deferred = mutable.ArrayBuffer.empty[PendingRequest]
    var selected = Option.empty[SimRequest]

    while (selected.isEmpty && pending.headOption.exists(_.request.cycle <= currentCycle)) {
      val candidate = pending.dequeue()
      if (requestQueues.canEnqueue(candidate.request.globalFlowId)) {
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
    Seq(cycleThread, admissionThread, dequeueThread).flatten.foreach(_.join())
    cycleThread = None
    admissionThread = None
    dequeueThread = None
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

    requestQueues.dequeue(globalFlowId, currentCycle) match {
      case Some(completed) =>
        val serializationCycles =
          math.max(1L, math.ceil(completed.request.sizeBytes / settings.linkBytesPerCycle).toLong)
        nextDequeueCycle = currentCycle + serializationCycles
        dequeueInFlight = false
        if (settings.verbose) {
          println(
            s"[RequestSim] completed cycle=$currentCycle requestId=${completed.request.requestId} " +
              s"flow=$globalFlowId size=${completed.request.sizeBytes} nextDequeue=$nextDequeueCycle"
          )
        }
      case None =>
        fail(
          new IllegalStateException(
            s"mesh popped flow $globalFlowId at cycle $currentCycle, but its simulator request queue is empty"
          )
        )
    }
  }

  private def isFinished: Boolean =
    inputClosed && pending.isEmpty && requestQueues.isEmpty && !dequeueInFlight &&
      requestQueues.snapshot.completedRequests == submittedIds.size

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

  def loadControlFile(path: Path, controller: PifoMeshSimController): Unit = {
    Files.readAllLines(path, StandardCharsets.UTF_8).asScala.zipWithIndex.foreach { case (raw, index) =>
      try {
        UnixDomainSocketLineServer.parseKeyValueLine(raw).foreach { line =>
          val unknown = line.fields.keySet.diff(commandFields)
          require(unknown.isEmpty, s"unknown fields: ${unknown.toSeq.sorted.mkString(", ")}")
          val commandName = line.requireString("command")
          val command = commandByName.getOrElse(
            commandName,
            throw new IllegalArgumentException(s"unknown command '$commandName'")
          )
          controller.sendControl(
            command,
            line.requireInt("engineId"),
            line.requireInt("data"),
            vPifoId = line.requireInt("vPifoId"),
            flowId = line.requireInt("flowId")
          )
        }
      } catch {
        case error: IllegalArgumentException =>
          throw new IllegalArgumentException(s"$path:${index + 1}: ${error.getMessage}", error)
      }
    }
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
        flowId = controller.mkFlowId(rootEngineId, globalFlowId)
      )
    }
    controller.sendControl(
      ControlCommand.UpdateMapperNonExist,
      rootEngineId,
      controller.mkFlowId(0, config.numVPIFOs - 1),
      vPifoId = rootVPifoId
    )
  }
}
