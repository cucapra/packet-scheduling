package rio.sim

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** One request arrival in a request trace.
  *
  * `globalFlowId` is the raw, globally unique flow identifier carried in the low bits of a PIFO token. The request
  * itself remains in a simulator-side queue because the current RTL deliberately carries only that compact token.
  */
case class SimRequest(cycle: Long, requestId: Long, globalFlowId: Int, sizeBytes: Int) {
  require(cycle >= 0, s"request cycle must be non-negative, got $cycle")
  require(requestId >= 0, s"requestId must be non-negative, got $requestId")
  require(globalFlowId >= 0, s"globalFlowId must be non-negative, got $globalFlowId")
  require(sizeBytes > 0, s"sizeBytes must be positive, got $sizeBytes")
}

case class QueuedRequest(request: SimRequest, admittedCycle: Long)

case class CompletedRequest(request: SimRequest, admittedCycle: Long, completedCycle: Long) {
  require(completedCycle >= admittedCycle, "a request cannot complete before it is admitted")

  def admissionDelayCycles: Long = admittedCycle - request.cycle
  def sojournCycles: Long = completedCycle - request.cycle
}

case class RequestQueueSnapshot(
    queuedRequests: Long,
    queuedBytes: Long,
    admittedRequests: Long,
    completedRequests: Long,
    completedBytes: Long
)

/** Bounded simulator-side FIFO queues, one queue per global flow ID. */
final class RequestQueueBank(val perFlowDepth: Int, val maxGlobalFlowId: Int) {
  require(perFlowDepth > 0, s"perFlowDepth must be positive, got $perFlowDepth")
  require(maxGlobalFlowId >= 0, s"maxGlobalFlowId must be non-negative, got $maxGlobalFlowId")

  private val queues = mutable.Map.empty[Int, mutable.Queue[QueuedRequest]]
  private val admittedIds = mutable.Set.empty[Long]
  private val completedBuffer = mutable.ArrayBuffer.empty[CompletedRequest]
  private var queuedByteCount = 0L
  private var admittedCount = 0L
  private var completedCount = 0L
  private var completedByteSum = 0L

  def canEnqueue(globalFlowId: Int): Boolean = {
    validateFlowId(globalFlowId)
    queues.get(globalFlowId).forall(_.size < perFlowDepth)
  }

  def enqueue(request: SimRequest, admittedCycle: Long): Unit = {
    validateFlowId(request.globalFlowId)
    require(admittedCycle >= request.cycle, s"request ${request.requestId} cannot be admitted before it arrives")
    require(!admittedIds.contains(request.requestId), s"duplicate requestId ${request.requestId}")

    val queue = queues.getOrElseUpdate(request.globalFlowId, mutable.Queue.empty)
    require(
      queue.size < perFlowDepth,
      s"request queue for flow ${request.globalFlowId} is full (depth=$perFlowDepth)"
    )

    queue.enqueue(QueuedRequest(request, admittedCycle))
    admittedIds += request.requestId
    admittedCount += 1
    queuedByteCount += request.sizeBytes
  }

  def dequeue(globalFlowId: Int, completedCycle: Long): Option[CompletedRequest] = {
    validateFlowId(globalFlowId)
    queues.get(globalFlowId).flatMap { queue =>
      if (queue.isEmpty) {
        None
      } else {
        val queued = queue.dequeue()
        val completed = CompletedRequest(queued.request, queued.admittedCycle, completedCycle)
        if (queue.isEmpty) queues -= globalFlowId
        queuedByteCount -= queued.request.sizeBytes
        completedCount += 1
        completedByteSum += queued.request.sizeBytes
        completedBuffer += completed
        Some(completed)
      }
    }
  }

  def queuedForFlow(globalFlowId: Int): Int = {
    validateFlowId(globalFlowId)
    queues.get(globalFlowId).fold(0)(_.size)
  }

  def activeFlowIds: Seq[Int] =
    queues.iterator.collect { case (flowId, queue) if queue.nonEmpty => flowId }.toSeq.sorted

  def totalQueued: Long = admittedCount - completedCount

  def isEmpty: Boolean = totalQueued == 0

  def completions: Vector[CompletedRequest] = completedBuffer.toVector

  def snapshot: RequestQueueSnapshot = RequestQueueSnapshot(
    queuedRequests = totalQueued,
    queuedBytes = queuedByteCount,
    admittedRequests = admittedCount,
    completedRequests = completedCount,
    completedBytes = completedByteSum
  )

  private def validateFlowId(globalFlowId: Int): Unit = {
    require(
      globalFlowId >= 0 && globalFlowId <= maxGlobalFlowId,
      s"globalFlowId $globalFlowId is outside [0, $maxGlobalFlowId]"
    )
  }
}

/** Canonical request trace format.
  *
  * The file is CSV with the exact header below. Blank lines and lines beginning with `#` are ignored. Numeric fields
  * accept decimal or Java-style prefixes such as `0x`.
  *
  * {{
  * cycle,request_id,global_flow_id,size_bytes
  * 0,1,1,64
  * 10,2,2,1500
  * }}
  */
object RequestTrace {
  val Header: Vector[String] = Vector("cycle", "request_id", "global_flow_id", "size_bytes")

  def load(path: Path): Vector[SimRequest] = {
    val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala
    parse(lines, path.toString)
  }

  def parse(lines: Iterable[String], origin: String = "<trace>"): Vector[SimRequest] = {
    val meaningful = lines.iterator.zipWithIndex.collect {
      case (raw, index) if raw.trim.nonEmpty && !raw.trim.startsWith("#") => (raw.stripPrefix("\ufeff"), index + 1)
    }.toVector

    require(meaningful.nonEmpty, s"$origin: empty request trace")
    val (headerText, headerLine) = meaningful.head
    val header = parseCsvLine(headerText, origin, headerLine).map(_.trim)
    require(
      header == Header,
      s"$origin:$headerLine: expected header '${Header.mkString(",")}', got '${header.mkString(",")}'"
    )

    val seenIds = mutable.Set.empty[Long]
    var previousCycle = -1L
    meaningful.tail.map { case (line, lineNumber) =>
      val fields = parseCsvLine(line, origin, lineNumber).map(_.trim)
      require(
        fields.length == Header.length,
        s"$origin:$lineNumber: expected ${Header.length} columns, got ${fields.length}"
      )

      val request =
        try {
          SimRequest(
            cycle = decodeLong(fields(0)),
            requestId = decodeLong(fields(1)),
            globalFlowId = decodeInt(fields(2)),
            sizeBytes = decodeInt(fields(3))
          )
        } catch {
          case error: IllegalArgumentException =>
            throw new IllegalArgumentException(s"$origin:$lineNumber: ${error.getMessage}", error)
        }

      require(!seenIds.contains(request.requestId), s"$origin:$lineNumber: duplicate request_id ${request.requestId}")
      require(
        request.cycle >= previousCycle,
        s"$origin:$lineNumber: cycles must be nondecreasing ($previousCycle followed by ${request.cycle})"
      )
      seenIds += request.requestId
      previousCycle = request.cycle
      request
    }
  }

  def write(path: Path, requests: Iterable[SimRequest]): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    val writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    try {
      writer.write(Header.mkString(","))
      writer.newLine()
      requests.foreach { request =>
        writer.write(
          Seq(request.cycle, request.requestId, request.globalFlowId, request.sizeBytes).mkString(",")
        )
        writer.newLine()
      }
    } finally writer.close()
  }

  def writeResults(path: Path, completed: Iterable[CompletedRequest]): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    val writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    try {
      writer.write(
        "request_id,global_flow_id,size_bytes,arrival_cycle,admitted_cycle,completed_cycle,admission_delay_cycles,sojourn_cycles"
      )
      writer.newLine()
      completed.foreach { result =>
        writer.write(
          Seq(
            result.request.requestId,
            result.request.globalFlowId,
            result.request.sizeBytes,
            result.request.cycle,
            result.admittedCycle,
            result.completedCycle,
            result.admissionDelayCycles,
            result.sojournCycles
          ).mkString(",")
        )
        writer.newLine()
      }
    } finally writer.close()
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

  private def parseCsvLine(line: String, origin: String, lineNumber: Int): Vector[String] = {
    val fields = mutable.ArrayBuffer.empty[String]
    val current = new StringBuilder
    var quoted = false
    var index = 0

    while (index < line.length) {
      line.charAt(index) match {
        case '"' if quoted && index + 1 < line.length && line.charAt(index + 1) == '"' =>
          current += '"'
          index += 1
        case '"' => quoted = !quoted
        case ',' if !quoted =>
          fields += current.toString
          current.clear()
        case char => current += char
      }
      index += 1
    }
    require(!quoted, s"$origin:$lineNumber: unterminated quoted CSV field")
    fields += current.toString
    fields.toVector
  }
}
