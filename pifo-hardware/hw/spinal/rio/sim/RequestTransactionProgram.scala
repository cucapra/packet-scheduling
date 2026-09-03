package rio.sim

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class RequestHardwareShape(
    numEngines: Int,
    numVPIFOs: Int,
    maxPacketPriority: Int,
    fifoDepth: Int,
    prefetchBufferDepth: Int
) {
  require(numEngines > 0, "numEngines must be positive")
  require(numVPIFOs >= 3, "numVPifos must be at least 3")
  require(maxPacketPriority > 1, "maxPacketPriority must be greater than one")
  require(fifoDepth > 0, "fifoDepth must be positive")
  require(prefetchBufferDepth > 0, "prefetchBufferDepth must be positive")
  private val capacity = numVPIFOs.toLong * fifoDepth
  require((capacity & (capacity - 1)) == 0, "numVPifos * fifoDepth must be a power of two")
}

case class ScheduledControlTransaction(
    scheduledCycle: Long,
    name: String,
    mode: String,
    before: String,
    after: String,
    drainTarget: Option[TreeDrainTarget],
    gatedFlowIds: Set[Int],
    minimumStopCycles: Long,
    instructions: Vector[RequestControlInstruction]
) {
  require(scheduledCycle >= 0, "transaction cycle must be non-negative")
  require(name.nonEmpty, "transaction name must not be empty")
  require(
    Set("direct", "in_place", "stop_the_world", "full_transitive", "confined_transitive").contains(mode),
    "invalid transaction mode"
  )
  require(
    !Set("full_transitive", "confined_transitive").contains(mode) || drainTarget.nonEmpty,
    s"$mode requires drainRoot"
  )
  require(gatedFlowIds.forall(_ >= 0), "gateFlows IDs must be non-negative")
  require(minimumStopCycles >= 0, "minStopCycles must be non-negative")
  require(
    mode == "stop_the_world" || minimumStopCycles == 0,
    "minStopCycles is only valid for stop_the_world"
  )
}

case class RequestTransactionProgram(
    hardware: RequestHardwareShape,
    rootEngineId: Int,
    rootVPifoId: Int,
    initialInstructions: Vector[RequestControlInstruction],
    transactions: Vector[ScheduledControlTransaction]
) {
  require(rootEngineId >= 1 && rootEngineId <= hardware.numEngines, "rootEngine is out of range")
  require(rootVPifoId >= 0 && rootVPifoId < hardware.numVPIFOs, "rootVPifoId is out of range")
  require(
    transactions.map(_.name).distinct.size == transactions.size,
    "timed transaction names must be unique"
  )
  require(
    transactions.map(_.scheduledCycle) == transactions.map(_.scheduledCycle).sorted,
    "timed transactions must be ordered by cycle"
  )
  require(
    transactions.flatMap(_.gatedFlowIds).forall(_ < hardware.numVPIFOs - 1),
    "gateFlows contains a reserved or out-of-range flow ID"
  )
  private val allInstructions = initialInstructions ++ transactions.flatMap(_.instructions)
  require(
    allInstructions.forall(instruction => instruction.engineId >= 1 && instruction.engineId <= hardware.numEngines),
    "transaction command engineId is out of range"
  )
  require(
    allInstructions.forall(instruction => instruction.vPifoId >= 0 && instruction.vPifoId < hardware.numVPIFOs),
    "transaction command vPifoId is out of range"
  )
}

object RequestTransactionProgram {
  private val Schema = "pifo-transactions-v1"
  private val HeaderFields = Set(
    "schema",
    "rootEngine",
    "rootVPifoId",
    "numEngines",
    "numVPifos",
    "maxPacketPriority",
    "fifoDepth",
    "prefetchBufferDepth"
  )
  private val MetadataFields =
    Set("at", "name", "mode", "before", "after", "drainRoot", "gateFlows", "minStopCycles")
  private val CommandFields = Set("command", "engineId", "vPifoId", "flowId", "data")
  private val TransactionFields = MetadataFields ++ CommandFields

  private case class Metadata(
      cycle: Option[Long],
      atText: String,
      name: String,
      mode: String,
      before: String,
      after: String,
      drainTarget: Option[TreeDrainTarget],
      gatedFlowIds: Set[Int],
      minimumStopCycles: Long
  )

  def load(path: Path): RequestTransactionProgram = {
    val records = Files
      .readAllLines(path, StandardCharsets.UTF_8)
      .asScala
      .zipWithIndex
      .flatMap { case (raw, index) =>
        try {
          UnixDomainSocketLineServer.parseKeyValueLine(raw).map(line => (index + 1, line.fields))
        } catch {
          case error: IllegalArgumentException =>
            throw new IllegalArgumentException(s"$path:${index + 1}: ${error.getMessage}", error)
        }
      }
      .toVector
    require(records.nonEmpty, s"$path: empty transaction program")

    val (headerLine, header) = records.head
    requireExactFields(path, headerLine, header, HeaderFields)
    require(header("schema") == Schema, s"$path:$headerLine: schema must be '$Schema'")
    val hardware = RequestHardwareShape(
      numEngines = parseInt(header("numEngines"), path, headerLine),
      numVPIFOs = parseInt(header("numVPifos"), path, headerLine),
      maxPacketPriority = parseInt(header("maxPacketPriority"), path, headerLine),
      fifoDepth = parseInt(header("fifoDepth"), path, headerLine),
      prefetchBufferDepth = parseInt(header("prefetchBufferDepth"), path, headerLine)
    )

    val groups = mutable.ArrayBuffer.empty[(Metadata, mutable.ArrayBuffer[RequestControlInstruction])]
    val seen = mutable.Set.empty[(String, String)]
    records.tail.foreach { case (lineNumber, fields) =>
      val unknown = fields.keySet.diff(TransactionFields)
      require(unknown.isEmpty, s"$path:$lineNumber: unknown fields: ${unknown.toSeq.sorted.mkString(", ")}")
      val required = Set("at", "name", "mode") ++ CommandFields
      val missing = required.diff(fields.keySet)
      require(missing.isEmpty, s"$path:$lineNumber: missing fields: ${missing.toSeq.sorted.mkString(", ")}")

      val atText = fields("at")
      val cycle = if (atText == "init") None else Some(parseLong(atText, path, lineNumber))
      cycle.foreach(value => require(value >= 0, s"$path:$lineNumber: transaction cycle must be non-negative"))
      val mode = fields("mode")
      require(
        Set("direct", "in_place", "stop_the_world", "full_transitive", "confined_transitive").contains(mode),
        s"$path:$lineNumber: unsupported transaction mode '$mode'"
      )
      val drainTarget = fields.get("drainRoot").map(value => parseDrainTarget(value, path, lineNumber))
      val gatedFlowIds = fields.get("gateFlows").map(value => parseFlowIds(value, path, lineNumber)).getOrElse(Set.empty)
      val minimumStopCycles = fields.get("minStopCycles").map(value => parseLong(value, path, lineNumber)).getOrElse(0L)
      val metadata = Metadata(
        cycle = cycle,
        atText = atText,
        name = fields("name"),
        mode = mode,
        before = fields.getOrElse("before", ""),
        after = fields.getOrElse("after", ""),
        drainTarget = drainTarget,
        gatedFlowIds = gatedFlowIds,
        minimumStopCycles = minimumStopCycles
      )
      val key = atText -> metadata.name
      if (groups.isEmpty || groups.last._1 != metadata) {
        require(!seen.contains(key), s"$path:$lineNumber: transaction '${metadata.name}' is not contiguous")
        groups += metadata -> mutable.ArrayBuffer.empty[RequestControlInstruction]
        seen += key
      }
      val instructionFields = fields.filter { case (key, _) => CommandFields.contains(key) }
      groups.last._2 += RequestSimulationConfiguration.parseControlInstruction(
        instructionFields,
        s"$path:$lineNumber"
      )
    }

    val parsed = groups.toVector.map { case (metadata, instructions) =>
      val packageInstructions = instructions.toVector
      RequestSimulationConfiguration.validateTransactionPackage(packageInstructions)
      metadata -> packageInstructions
    }
    val initial = parsed.filter(_._1.cycle.isEmpty)
    require(initial.size <= 1, s"$path: expected at most one at=init package")
    require(
      parsed.dropWhile(_._1.cycle.isEmpty).forall(_._1.cycle.nonEmpty),
      s"$path: at=init package must precede timed transactions"
    )
    initial.headOption.foreach { case (metadata, _) =>
      require(metadata.mode == "direct", s"$path: at=init package mode must be direct")
      require(metadata.drainTarget.isEmpty, s"$path: at=init package cannot have drainRoot")
      require(metadata.minimumStopCycles == 0, s"$path: at=init package cannot have minStopCycles")
    }
    val timed = parsed.collect { case (metadata, instructions) if metadata.cycle.nonEmpty =>
      ScheduledControlTransaction(
        scheduledCycle = metadata.cycle.get,
        name = metadata.name,
        mode = metadata.mode,
        before = metadata.before,
        after = metadata.after,
        drainTarget = metadata.drainTarget,
        gatedFlowIds = metadata.gatedFlowIds,
        minimumStopCycles = metadata.minimumStopCycles,
        instructions = instructions
      )
    }
    RequestTransactionProgram(
      hardware = hardware,
      rootEngineId = parseInt(header("rootEngine"), path, headerLine),
      rootVPifoId = parseInt(header("rootVPifoId"), path, headerLine),
      initialInstructions = initial.headOption.map(_._2).getOrElse(Vector.empty),
      transactions = timed
    )
  }

  private def requireExactFields(
      path: Path,
      lineNumber: Int,
      fields: Map[String, String],
      expected: Set[String]
  ): Unit = {
    val missing = expected.diff(fields.keySet)
    val unknown = fields.keySet.diff(expected)
    require(missing.isEmpty, s"$path:$lineNumber: missing fields: ${missing.toSeq.sorted.mkString(", ")}")
    require(unknown.isEmpty, s"$path:$lineNumber: unknown fields: ${unknown.toSeq.sorted.mkString(", ")}")
  }

  private def parseDrainTarget(value: String, path: Path, lineNumber: Int): TreeDrainTarget = {
    value.split(":", 2) match {
      case Array(engineId, vPifoId) =>
        TreeDrainTarget(parseInt(engineId, path, lineNumber), parseInt(vPifoId, path, lineNumber))
      case _ => throw new IllegalArgumentException(s"$path:$lineNumber: drainRoot must be ENGINE:VPIFO")
    }
  }

  private def parseFlowIds(value: String, path: Path, lineNumber: Int): Set[Int] = {
    val result = value.split(",").iterator.map(item => parseInt(item, path, lineNumber)).toSet
    require(result.nonEmpty, s"$path:$lineNumber: gateFlows must contain at least one flow ID")
    require(result.forall(_ >= 0), s"$path:$lineNumber: gateFlows IDs must be non-negative")
    result
  }

  private def parseLong(value: String, path: Path, lineNumber: Int): Long = {
    try java.lang.Long.decode(value).longValue()
    catch {
      case _: NumberFormatException =>
        throw new IllegalArgumentException(s"$path:$lineNumber: invalid integer '$value'")
    }
  }

  private def parseInt(value: String, path: Path, lineNumber: Int): Int = {
    val decoded = parseLong(value, path, lineNumber)
    require(
      decoded >= Int.MinValue && decoded <= Int.MaxValue,
      s"$path:$lineNumber: integer '$value' does not fit in 32 bits"
    )
    decoded.toInt
  }
}
