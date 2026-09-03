package rio.sim

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import spinal.core._
import spinal.core.sim._

import rio._

case class RequestSimulatorOptions(
    traceFile: Option[Path] = None,
    liveRequests: Boolean = false,
    requestSocketPath: String = SimUtils.DefaultRequestSocketPath,
    controlFile: Option[Path] = None,
    controlSocketEnabled: Boolean = true,
    controlSocketPath: String = SimUtils.DefaultControlSocketPath,
    resultFile: Option[Path] = Some(Paths.get("request-results.csv")),
    packetOutcomeFile: Option[Path] = None,
    flatFifo: Option[Boolean] = None,
    flatFifoFlows: Set[Int] = Set.empty,
    rootEngineId: Int = 1,
    rootVPifoId: Int = 10,
    perFlowQueueDepth: Int = 32,
    linkBytesPerCycle: Double = 64.0,
    maxCycles: Long = 100000L,
    warmupCycles: Int = 4,
    numEngines: Int = 2,
    numVPIFOs: Int = 32,
    maxPacketPriority: Int = 256,
    fifoDepth: Int = 32,
    prefetchBufferDepth: Int = 2,
    waveEnabled: Boolean = true,
    verbose: Boolean = true,
    transactionProgramFile: Option[Path] = None,
    transactionEventFile: Option[Path] = None
)

/** Full request-level PIFO mesh simulation CLI. */
object RequestSimulatorCli {
  private object HelpRequested extends RuntimeException

  private val Usage =
    """Usage: sbt 'runMain rio.sim.RequestSimulatorCli [options]'
      |
      |Workload:
      |  --trace FILE                 Play a canonical request trace CSV.
      |  --live                       Accept requests on a Unix socket until command=end.
      |  --request-socket PATH        Request socket (default /tmp/rio-request.sock).
      |  --output FILE                Completion CSV (default request-results.csv).
      |  --packet-outcomes FILE       Per-packet push/pop/drop CSV.
      |  --no-output                  Do not write a completion CSV.
      |
      |Scheduler configuration:
      |  --transactions FILE          Initial and timed direct command packages.
      |  --control-file FILE          Apply key=value control commands before workload cycle 0.
      |  --control-socket PATH        Online control socket (default /tmp/rio-control.sock).
      |  --no-control-socket          Disable online control instructions.
      |  --flat-fifo                  Also configure the root as a flat FIFO.
      |  --no-flat-fifo               Do not auto-configure a flat FIFO.
      |  --flat-fifo-flows IDS        Comma-separated global flow IDs for live flat-FIFO runs.
      |  --root-engine ID             Root engine (default 1).
      |  --root-vpifo ID              Root virtual PIFO (default 10).
      |  --transaction-event-output F Write start/commit/finish/drain cycles as CSV.
      |
      |Request model:
      |  --queue-depth N              Simulator FIFO depth per global flow (default 32).
      |  --link-bytes-per-cycle N     Output-link service rate (default 64.0).
      |  --max-cycles N               Timeout in workload cycles (default 100000).
      |  --warmup-cycles N            Cycles after configuration and before cycle 0 (default 4).
      |
      |Hardware shape:
      |  --num-engines N              Default 2.
      |  --num-vpifos N               Default 32; the highest ID is reserved as empty.
      |  --max-packet-priority N       Default 256.
      |  --fifo-depth N                Default 32.
      |  --prefetch-buffer-depth N     Default 2.
      |  --no-wave                     Disable FST waveform generation.
      |  --quiet                       Suppress request admission/completion logs.
      |  --help                        Show this help.
      |
      |Canonical trace header:
      |  cycle,request_id,global_flow_id,size_bytes
      |
      |The --transactions file starts with a pifo-transactions-v1 hardware header.
      |Every following line is one exact command tagged with at=init or at=<cycle>
      |and a transaction name. Each package ends with exactly one CommitMapper.
      |
      |If neither --control-file nor an at=init package nor an explicit flat-FIFO flag is supplied, a flat FIFO is configured automatically.
      |For --live without an input trace, all usable global flow IDs are configured unless --flat-fifo-flows is given.
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    try run(parse(args))
    catch {
      case HelpRequested => println(Usage)
      case error: IllegalArgumentException =>
        Console.err.println(s"error: ${error.getMessage}\n")
        Console.err.println(Usage)
        sys.exit(2)
    }
  }

  private def run(requestedOptions: RequestSimulatorOptions): Unit = {
    val transactionProgram = requestedOptions.transactionProgramFile.map(RequestTransactionProgram.load)
    require(
      transactionProgram.isEmpty || requestedOptions.controlFile.isEmpty,
      "--transactions cannot be combined with --control-file"
    )
    val options = transactionProgram match {
      case Some(program) =>
        requestedOptions.copy(
          rootEngineId = program.rootEngineId,
          rootVPifoId = program.rootVPifoId,
          numEngines = program.hardware.numEngines,
          numVPIFOs = program.hardware.numVPIFOs,
          maxPacketPriority = program.hardware.maxPacketPriority,
          fifoDepth = program.hardware.fifoDepth,
          prefetchBufferDepth = program.hardware.prefetchBufferDepth
        )
      case None => requestedOptions
    }
    require(options.traceFile.nonEmpty || options.liveRequests, "provide --trace FILE, --live, or both")
    require(options.numEngines > 0, "--num-engines must be positive")
    require(options.numVPIFOs >= 3, "--num-vpifos must be at least 3")
    require(options.rootEngineId >= 1 && options.rootEngineId <= options.numEngines, "root engine is out of range")
    require(options.rootVPifoId >= 0 && options.rootVPifoId < options.numVPIFOs, "root vPifo is out of range")
    require(options.warmupCycles >= 0, "--warmup-cycles must be non-negative")
    require(options.perFlowQueueDepth > 0, "--queue-depth must be positive")
    require(
      java.lang.Double.isFinite(options.linkBytesPerCycle) && options.linkBytesPerCycle > 0,
      "--link-bytes-per-cycle must be finite and positive"
    )
    require(options.maxCycles > 0, "--max-cycles must be positive")
    require(options.maxPacketPriority > 1, "--max-packet-priority must be greater than one")
    require(options.fifoDepth > 0, "--fifo-depth must be positive")
    require(options.prefetchBufferDepth > 0, "--prefetch-buffer-depth must be positive")
    require(
      options.transactionEventFile.isEmpty || transactionProgram.exists(_.transactions.nonEmpty),
      "--transaction-event-output requires timed --transactions"
    )
    val pifoCapacity = options.numVPIFOs.toLong * options.fifoDepth
    require((pifoCapacity & (pifoCapacity - 1)) == 0, "--num-vpifos times --fifo-depth must be a power of two")

    val scheduledTransactions = transactionProgram.toVector.flatMap(_.transactions)
    scheduledTransactions.foreach { transaction =>
      require(transaction.scheduledCycle < options.maxCycles, "transaction cycle must be less than --max-cycles")
      transaction.drainTarget.foreach { target =>
        require(target.engineId <= options.numEngines, "drainRoot engine is out of range")
        require(target.vPifoId < options.numVPIFOs, "drainRoot vPifo is out of range")
      }
    }

    val trace = options.traceFile.map(RequestTrace.load).getOrElse(Vector.empty)
    trace.foreach { request =>
      require(
        request.globalFlowId < options.numVPIFOs - 1,
        s"trace flow ${request.globalFlowId} does not fit; ${options.numVPIFOs - 1} is reserved as the empty token"
      )
    }
    val traceFlows = trace.iterator.map(_.globalFlowId).toSet
    val configuredFlows = options.flatFifoFlows ++ traceFlows match {
      case flows if flows.nonEmpty => flows
      case _                       => (0 until options.numVPIFOs - 1).toSet
    }

    val hardwareConfig = EngineConfig(
      numEngines = options.numEngines,
      numVPIFOs = options.numVPIFOs,
      maxPacketPriority = options.maxPacketPriority,
      fifoDepth = options.fifoDepth,
      prefetchBufferDepth = options.prefetchBufferDepth
    )

    val baseSimConfig = SimConfig.withIVerilog.addSimulatorFlag("-g2012")
    val selectedSimConfig = if (options.waveEnabled) baseSimConfig.withFstWave else baseSimConfig

    selectedSimConfig
      .compile {
        val mesh = new PifoMesh(hardwareConfig)
        mesh.pifoEngines.foreach { engine =>
          engine.pifos.io.popResponse.valid.simPublic()
          engine.pifos.io.popResponse.port.simPublic()
          engine.pifos.io.popResponse.exist.simPublic()
          engine.pifos.io.popResponse.data.simPublic()
          engine.pifos.io.popResponse.priority.simPublic()
          engine.pifos.io.popPortEmpty.simPublic()
          engine.pifos.io.portDrained.valid.simPublic()
          engine.pifos.io.portDrained.payload.simPublic()
        }
        mesh
      }
      .doSim { dut =>
        val controller = PifoMeshSimController(hardwareConfig, dut)
        controller.start(options.controlSocketEnabled, options.controlSocketPath, monitorPops = options.verbose)

        options.controlFile.foreach { path =>
          println(s"[RequestSim] applying control commands from $path")
          RequestSimulationConfiguration.loadControlFile(path, controller)
        }
        transactionProgram.filter(_.initialInstructions.nonEmpty).foreach { program =>
          println(s"[RequestSim] applying initial package from ${options.transactionProgramFile.get}")
          RequestSimulationConfiguration.executeTransactionPackage(
            program.initialInstructions,
            controller,
            beforeCommit = () => (),
            markCommitAccepted = () => (),
            onCommitApplied = () => ()
          )
        }

        val hasInitialPackage = transactionProgram.exists(_.initialInstructions.nonEmpty)
        val useFlatFifo = options.flatFifo.getOrElse(options.controlFile.isEmpty && !hasInitialPackage)
        if (useFlatFifo) {
          println(
            s"[RequestSim] configuring flat FIFO at engine=${options.rootEngineId} vPifo=${options.rootVPifoId} " +
              s"for flows=${configuredFlows.toSeq.sorted.mkString(",")}"
          )
          RequestSimulationConfiguration.configureFlatFifo(
            hardwareConfig,
            controller,
            options.rootEngineId,
            options.rootVPifoId,
            configuredFlows
          )
        }

        if (options.warmupCycles > 0) dut.clockDomain.waitRisingEdge(options.warmupCycles)

        val scheduledActions = scheduledTransactions.map { transaction =>
          ScheduledRequestAction(
            scheduledCycle = transaction.scheduledCycle,
            name = transaction.name,
            drainTarget = transaction.drainTarget,
            mode = transaction.mode,
            gatedFlowIds = transaction.gatedFlowIds,
            minimumStopCycles = transaction.minimumStopCycles,
            run = context => {
              if (transaction.mode == "stop_the_world") {
                context.beginStopTheWorld(transaction.minimumStopCycles)
              }
              RequestSimulationConfiguration.executeTransactionPackage(
                transaction.instructions,
                controller,
                context.beforeCommit,
                context.markCommitAccepted,
                context.markCommitApplied
              )
              if (transaction.mode == "stop_the_world") {
                context.finishStopTheWorld()
              }
            }
          )
        }

        val requestSimulator = new PifoRequestSimulator(
          hardwareConfig,
          dut,
          controller,
          RequestSimulationSettings(
            rootEngineId = options.rootEngineId,
            rootVPifoId = options.rootVPifoId,
            perFlowQueueDepth = options.perFlowQueueDepth,
            linkBytesPerCycle = options.linkBytesPerCycle,
            maxCycles = options.maxCycles,
            verbose = options.verbose
          ),
          scheduledActions
        )

        requestSimulator.submitAll(trace)
        if (options.liveRequests) {
          requestSimulator.startRequestSocket(options.requestSocketPath)
        } else {
          requestSimulator.closeInput()
        }

        val summary = requestSimulator.run()
        options.resultFile.foreach(path => RequestTrace.writeResults(path, summary.completions))
        options.packetOutcomeFile.foreach(path =>
          RequestTrace.writePacketOutcomes(path, summary.completions, summary.drops)
        )
        val completedTransactions = scheduledTransactions.map { transaction =>
          val action = summary.completedActions
            .find(_.name == transaction.name)
            .getOrElse(
              throw new IllegalStateException(s"transaction '${transaction.name}' did not complete")
            )
          val commitCycle = action.commitCycle.getOrElse(
            throw new IllegalStateException(s"transaction '${transaction.name}' did not accept CommitMapper")
          )
          val drainText = action.drainCycle.map(cycle => s" drain=$cycle").getOrElse("")
          val dropText = if (action.droppedPackets > 0) s" dropped=${action.droppedPackets}" else ""
          val stopText = if (transaction.mode == "stop_the_world") {
            s" retained=${action.retainedPackets} peakBuffer=${action.peakBufferOccupancyPackets} " +
              s"minStop=${transaction.minimumStopCycles}"
          } else ""
          println(
            s"[RequestSim] transaction ${transaction.name} mode=${transaction.mode} " +
              s"scheduled=${action.scheduledCycle} start=${action.startCycle} " +
              s"instructions=${transaction.instructions.size} commit=$commitCycle " +
              s"finish=${action.finishCycle}$drainText$dropText$stopText"
          )
          (transaction, action)
        }
        options.transactionEventFile.foreach(path => writeTransactionEvents(path, completedTransactions))
        println(
          s"[RequestSim] complete cycles=${summary.elapsedCycles} submitted=${summary.submittedRequests} " +
            s"admitted=${summary.admittedRequests} completed=${summary.completedRequests} " +
              s"dropped=${summary.droppedRequests} bytes=${summary.completedBytes}"
        )
        options.resultFile.foreach(path => println(s"[RequestSim] wrote completion trace to $path"))
        options.packetOutcomeFile.foreach(path => println(s"[RequestSim] wrote packet outcomes to $path"))
        options.transactionEventFile.foreach(path => println(s"[RequestSim] wrote transaction event to $path"))
        simSuccess()
      }
  }

  private def parse(args: Array[String]): RequestSimulatorOptions = {
    var options = RequestSimulatorOptions()
    var index = 0

    def nextValue(flag: String): String = {
      require(index + 1 < args.length, s"$flag requires a value")
      index += 1
      args(index)
    }

    while (index < args.length) {
      args(index) match {
        case "--trace"             => options = options.copy(traceFile = Some(Paths.get(nextValue("--trace"))))
        case "--live"              => options = options.copy(liveRequests = true)
        case "--request-socket"    => options = options.copy(requestSocketPath = nextValue("--request-socket"))
        case "--transactions" =>
          options = options.copy(transactionProgramFile = Some(Paths.get(nextValue("--transactions"))))
        case "--control-file"      => options = options.copy(controlFile = Some(Paths.get(nextValue("--control-file"))))
        case "--control-socket"    => options = options.copy(controlSocketPath = nextValue("--control-socket"))
        case "--no-control-socket" => options = options.copy(controlSocketEnabled = false)
        case "--output"            => options = options.copy(resultFile = Some(Paths.get(nextValue("--output"))))
        case "--packet-outcomes" =>
          options = options.copy(packetOutcomeFile = Some(Paths.get(nextValue("--packet-outcomes"))))
        case "--no-output"         => options = options.copy(resultFile = None)
        case "--flat-fifo"         => options = options.copy(flatFifo = Some(true))
        case "--no-flat-fifo"      => options = options.copy(flatFifo = Some(false))
        case "--flat-fifo-flows" => options = options.copy(flatFifoFlows = parseIntSet(nextValue("--flat-fifo-flows")))
        case "--root-engine"     => options = options.copy(rootEngineId = decodeInt(nextValue("--root-engine")))
        case "--root-vpifo"      => options = options.copy(rootVPifoId = decodeInt(nextValue("--root-vpifo")))
        case "--transaction-event-output" =>
          options = options.copy(transactionEventFile = Some(Paths.get(nextValue("--transaction-event-output"))))
        case "--queue-depth" => options = options.copy(perFlowQueueDepth = decodeInt(nextValue("--queue-depth")))
        case "--link-bytes-per-cycle" =>
          options = options.copy(linkBytesPerCycle = nextValue("--link-bytes-per-cycle").toDouble)
        case "--max-cycles"    => options = options.copy(maxCycles = decodeLong(nextValue("--max-cycles")))
        case "--warmup-cycles" => options = options.copy(warmupCycles = decodeInt(nextValue("--warmup-cycles")))
        case "--num-engines"   => options = options.copy(numEngines = decodeInt(nextValue("--num-engines")))
        case "--num-vpifos"    => options = options.copy(numVPIFOs = decodeInt(nextValue("--num-vpifos")))
        case "--max-packet-priority" =>
          options = options.copy(maxPacketPriority = decodeInt(nextValue("--max-packet-priority")))
        case "--fifo-depth" => options = options.copy(fifoDepth = decodeInt(nextValue("--fifo-depth")))
        case "--prefetch-buffer-depth" =>
          options = options.copy(prefetchBufferDepth = decodeInt(nextValue("--prefetch-buffer-depth")))
        case "--no-wave"     => options = options.copy(waveEnabled = false)
        case "--quiet"       => options = options.copy(verbose = false)
        case "--help" | "-h" => throw HelpRequested
        case unknown         => throw new IllegalArgumentException(s"unknown option '$unknown'")
      }
      index += 1
    }
    options
  }

  private def parseIntSet(value: String): Set[Int] = {
    val values = value.split(",").iterator.map(_.trim).filter(_.nonEmpty).map(decodeInt).toSet
    require(values.nonEmpty, "--flat-fifo-flows requires at least one ID")
    values
  }

  private def writeTransactionEvents(
      path: Path,
      completed: Seq[(ScheduledControlTransaction, CompletedRequestAction)]
  ): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    val writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    try {
      writer.write(
        "event,name,mode,from_policy,to_policy,instruction_count,scheduled_cycle,start_cycle,commit_cycle," +
          "finish_cycle,drain_cycle,drain_duration_cycles,dropped_packets,retained_packets," +
          "peak_buffer_occupancy_packets,minimum_stop_cycles,stop_duration_cycles"
      )
      writer.newLine()
      completed.foreach { case (transaction, action) =>
        val commitCycle = action.commitCycle.getOrElse(
          throw new IllegalArgumentException(
            s"transaction '${transaction.name}' is missing its CommitMapper cycle"
          )
        )
        val drainCycle = action.drainCycle.map(_.toString).getOrElse("")
        val drainDuration =
          if (transaction.mode == "stop_the_world") ""
          else action.drainCycle.map(_ - commitCycle).map(_.toString).getOrElse("")
        val stopDuration =
          if (transaction.mode == "stop_the_world") action.drainCycle.map(action.finishCycle - _).map(_.toString)
          else None
        writer.write(
          Seq(
            "reconfiguration",
            transaction.name,
            transaction.mode,
            transaction.before,
            transaction.after,
            transaction.instructions.size,
            action.scheduledCycle,
            action.startCycle,
            commitCycle,
            action.finishCycle,
            drainCycle,
            drainDuration,
            action.droppedPackets,
            action.retainedPackets,
            action.peakBufferOccupancyPackets,
            transaction.minimumStopCycles,
            stopDuration.getOrElse("")
          ).map(csvCell).mkString(",")
        )
        writer.newLine()
      }
    } finally writer.close()
  }

  private def csvCell(value: Any): String = {
    val raw = value.toString
    if (raw.exists(character => character == ',' || character == '"' || character == '\n' || character == '\r')) {
      "\"" + raw.replace("\"", "\"\"") + "\""
    } else raw
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
