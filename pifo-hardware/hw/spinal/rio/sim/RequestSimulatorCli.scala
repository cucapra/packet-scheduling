package rio.sim

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

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
    controlQueueDepth: Int = EngineConfig.DefaultCommitQueueLength,
    waveEnabled: Boolean = true,
    verbose: Boolean = true,
    transactionProgramFile: Option[Path] = None,
    transactionEventFile: Option[Path] = None,
    unadmittedFlows: Set[Int] = Set.empty,
    verilator: Boolean = false
)

/** Full request-level PIFO mesh simulation CLI. */
object RequestSimulatorCli {
  private object HelpRequested extends RuntimeException
  private type BuildKey = (EngineConfig, Boolean, Boolean, Boolean)
  private var batchModels: Option[scala.collection.mutable.Map[BuildKey, SimCompiled[PifoMesh]]] = None

  private val Usage =
    """Usage: sbt 'runMain rio.sim.RequestSimulatorCli [options]'
      |
      |Workload:
      |  --batch FILE                 One tab-separated CLI argument list per line; reuse matching builds.
      |  --trace FILE                 Play a canonical request trace CSV.
      |  --live                       Accept requests on a Unix socket until command=end.
      |  --request-socket PATH        Request socket (default /tmp/rio-request.sock).
      |  --output FILE                Completion CSV (default request-results.csv).
      |  --packet-outcomes FILE       Per-packet push/pop/drop CSV.
      |  --unadmitted-flows IDS       Control-only flows with no policy arm; log as unserved, not dropped.
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
      |  --control-queue-depth N       Shared command/replay FIFO, power of two (default 256).
      |  --no-wave                     Disable FST waveform generation.
      |  --quiet                       Suppress request admission/completion logs.
      |  --verilator                   Use Verilator (faster for large register trees).
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

  def main(args: Array[String]): Unit = run(args, evaluation = false)

  def run(args: Array[String], evaluation: Boolean): Unit = {
    try {
      if (args.headOption.contains("--batch")) {
        require(args.length == 2, "--batch requires exactly one argument-list file")
        val jobs = Files.readAllLines(Paths.get(args(1)), StandardCharsets.UTF_8).asScala
          .filter(line => line.nonEmpty && !line.startsWith("#")).map(line => parse(line.split("\t", -1)))
        require(jobs.nonEmpty, "batch contains no simulations")
        batchModels = Some(scala.collection.mutable.Map.empty)
        try jobs.zipWithIndex.foreach { case (options, index) =>
          println(s"[RequestBatch] start ${index + 1}/${jobs.size} output=${options.resultFile.getOrElse("")}")
          run(options, evaluation)
          println(s"[RequestBatch] done ${index + 1}/${jobs.size}")
        } finally batchModels = None
      } else run(parse(args), evaluation)
    }
    catch {
      case HelpRequested => println(Usage)
      case error: IllegalArgumentException =>
        Console.err.println(s"error: ${error.getMessage}\n")
        Console.err.println(Usage)
        sys.exit(2)
    }
  }

  private def run(requestedOptions: RequestSimulatorOptions, evaluation: Boolean): Unit = {
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
      prefetchBufferDepth = options.prefetchBufferDepth,
      commitQueueLength = options.controlQueueDepth
    )

    transactionProgram.foreach { program =>
      val instructions = program.initialInstructions ++ program.transactions.flatMap(_.instructions)
      instructions.foreach(i => SimUtils.requireSupportedCommand(i.command, i.data, evaluation))
      ReplayEpochCapacity.validate(hardwareConfig, program.initialInstructions.map(_.command))
      program.transactions.foreach(t => ReplayEpochCapacity.validate(hardwareConfig, t.instructions.map(_.command)))
    }
    options.controlFile.foreach { path =>
      val instructions = RequestSimulationConfiguration.loadControlInstructions(path)
      instructions.foreach(i => SimUtils.requireSupportedCommand(i.command, i.data, evaluation))
      ReplayEpochCapacity.validate(hardwareConfig, instructions.map(_.command))
    }
    val hasInitialPackage = transactionProgram.exists(_.initialInstructions.nonEmpty)
    val useFlatFifo = options.flatFifo.getOrElse(options.controlFile.isEmpty && !hasInitialPackage)
    if (useFlatFifo)
      ReplayEpochCapacity.validate(hardwareConfig, Seq.fill(configuredFlows.size * 2)(ControlCommand.UpdateMapperPre))

    // The evaluation ranker's indexed register writes decode port/flow pairs.
    val simConfig = SimConfig.withConfig(SpinalConfig(
      bitVectorWidthMax = math.max(4096, 1 << (2 * hardwareConfig.vpifoIdWidth))))
    val baseSimConfig = if (options.verilator) simConfig.withVerilator.workspaceName("PifoMeshVerilator")
      else simConfig.withIVerilog.addSimulatorFlag("-g2012")
    val selectedSimConfig = if (options.waveEnabled) baseSimConfig.withFstWave else baseSimConfig

    def compileModel(): SimCompiled[PifoMesh] = selectedSimConfig
      .compile {
        val mesh = if (evaluation) new EvaluationPifoMesh(hardwareConfig) else new PifoMesh(hardwareConfig)
        mesh.guard.emptyPifos.foreach(_.foreach(_.simPublic()))
        mesh.routedControl.simPublic()
        mesh.controlQueue.simPublic()
        mesh.commitControl.simPublic()
        if (evaluation) {
        mesh.maintenance.activeRootValid.simPublic()
        mesh.maintenance.activeRootEngine.simPublic()
        mesh.maintenance.activeRootPifo.simPublic()
        mesh.maintenance.pendingRootValid.simPublic()
        mesh.maintenance.pendingRootEngine.simPublic()
        mesh.maintenance.pendingRootPifo.simPublic()
        mesh.maintenance.trafficStopped.simPublic()
        mesh.maintenance.stoppedRootEngine.simPublic()
        mesh.maintenance.stoppedRootPifo.simPublic()
        mesh.maintenance.stopSnapshotValid.simPublic()
        mesh.maintenance.stoppedTokenCount.simPublic()
        mesh.maintenance.copyController.io.done.simPublic()
        mesh.maintenance.copyController.io.copied.simPublic()
        mesh.maintenance.copyController.source.simPublic()
        mesh.maintenance.copyController.target.simPublic()
        }
        mesh.pifoEngines.foreach { engine =>
          engine.pifos.pifoCount.simPublic()
          if (evaluation) {
          engine.pifos.portCounts.foreach(_.simPublic())
          engine.prefill.busy.simPublic()
          engine.prefill.request.simPublic()
          engine.pifos.io.push2.valid.simPublic()
          engine.pifos.io.push2Ready.simPublic()
          }
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
    val key = (hardwareConfig, evaluation, options.verilator, options.waveEnabled)
    val compiled = batchModels match {
      case Some(models) => models.getOrElseUpdate(key, compileModel())
      case None => compileModel()
    }
    // doSim constructs a fresh backend instance; only compilation is shared.
    compiled.doSim { dut =>
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
          val stopTheWorld = transaction.mode == "stop_the_world_pop"
          ScheduledRequestAction(
            scheduledCycle = transaction.scheduledCycle,
            name = transaction.name,
            drainTarget = transaction.drainTarget,
            mode = transaction.mode,
            gatedFlowIds = transaction.gatedFlowIds,
            minimumStopCycles = transaction.minimumStopCycles,
            isCleanup = transaction.cleanupOf.nonEmpty,
            run = context => {
              if (stopTheWorld) context.beginStopWorldPop()
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
              context.markCommitFinished()
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

        val (unadmitted, admittedTrace) = trace.partition(r => options.unadmittedFlows.contains(r.globalFlowId))
        requestSimulator.submitAll(admittedTrace)
        if (options.liveRequests) {
          requestSimulator.startRequestSocket(options.requestSocketPath)
        } else {
          requestSimulator.closeInput()
        }

        val summary = requestSimulator.run()
        options.resultFile.foreach(path => RequestTrace.writeResults(path, summary.completions))
        options.packetOutcomeFile.foreach(path =>
          RequestTrace.writePacketOutcomes(path, summary.completions, summary.drops, unadmitted)
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
          val cleanupAction = scheduledTransactions.find(_.cleanupOf.contains(transaction.name))
            .flatMap(cleanup => summary.completedActions.find(_.name == cleanup.name))
          val finish = cleanupAction.map(_.finishCycle).getOrElse(action.finishCycle)
          val dropText = if (action.droppedPackets > 0) s" dropped=${action.droppedPackets}" else ""
          val stopText = if (transaction.mode == "stop_the_world") {
            s" retained=${action.retainedPackets} peakBuffer=${action.peakBufferOccupancyPackets} " +
              s"minStop=${transaction.minimumStopCycles}"
          } else ""
          println(
            s"[RequestSim] transaction ${transaction.name} mode=${transaction.mode} " +
              s"scheduled=${action.scheduledCycle} start=${action.startCycle} " +
              s"instructions=${transaction.instructions.size} commit=$commitCycle " +
              s"applied=${action.commitAppliedCycle.get} " +
              s"commitCycles=${action.commitAppliedCycle.get - action.startCycle} " +
              s"finish=$finish (double-buffer cleanup done) installFinish=${action.finishCycle} " +
              s"bankCleanupCycles=${action.finishCycle - action.commitAppliedCycle.get}" +
              s"$drainText$dropText$stopText" + action.resumeCycle.map(c => s" resumed=$c").getOrElse("")
          )
          (transaction, action)
        }
        options.transactionEventFile.foreach(path => writeTransactionEvents(path, completedTransactions))
        options.resultFile.foreach { path =>
          val writer = Files.newBufferedWriter(path.resolveSibling("controller-instructions.csv"), StandardCharsets.UTF_8)
          try {
            writer.write("cycle,phase,command,engine_id,vpifo_id,flow_id,data,copied_entries\n")
            summary.controlObservations.foreach { observation =>
              writer.write(Seq(observation.cycle, observation.phase, observation.command, observation.engineId,
                observation.vPifoId, observation.flowId, observation.data,
                observation.copiedEntries.map(_.toString).getOrElse("")).mkString(",") + "\n")
            }
          } finally writer.close()
          val maintenance = Files.newBufferedWriter(path.resolveSibling("maintenance-events.csv"), StandardCharsets.UTF_8)
          try {
            maintenance.write("cycle,event,engine_id,vpifo_id,tokens,buffered_packets,ingress_packets\n")
            summary.maintenanceObservations.foreach { observation =>
              maintenance.write(Seq(observation.cycle, observation.event, observation.engineId,
                observation.vPifoId, observation.tokens, observation.bufferedPackets,
                observation.ingressPackets).mkString(",") + "\n")
            }
          } finally maintenance.close()
        }
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
        case "--unadmitted-flows" =>
          options = options.copy(unadmittedFlows = parseIntSet(nextValue("--unadmitted-flows")))
        case "--verilator" => options = options.copy(verilator = true)
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
        case "--control-queue-depth" =>
          options = options.copy(controlQueueDepth = decodeInt(nextValue("--control-queue-depth")))
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
          "peak_buffer_occupancy_packets,minimum_stop_cycles,stop_duration_cycles," +
          "commit_applied_cycle,commit_cycles,bank_cleanup_cycles,install_finish_cycle,resume_cycle," +
          "cleanup_start_cycle,cleanup_commit_cycle,cleanup_applied_cycle,cleanup_finish_cycle," +
          "cleanup_instruction_count,cleanup_commit_cycles,cleanup_bank_cleanup_cycles,cleanup_of,prefilled_tokens"
      )
      writer.newLine()
      completed.foreach { case (transaction, action) =>
        val cleanup = completed.find(_._1.cleanupOf.contains(transaction.name))
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
          if (transaction.mode == "stop_the_world") action.drainCycle.map(action.resumeCycle.get - _).map(_.toString)
          else None
        writer.write(
          Seq(
            if (transaction.cleanupOf.nonEmpty) "cleanup_commit" else "reconfiguration",
            transaction.name,
            transaction.mode,
            transaction.before,
            transaction.after,
            transaction.instructions.size,
            action.scheduledCycle,
            action.startCycle,
            commitCycle,
            cleanup.map(_._2.finishCycle).getOrElse(action.finishCycle),
            drainCycle,
            drainDuration,
            action.droppedPackets,
            action.retainedPackets,
            action.peakBufferOccupancyPackets,
            transaction.minimumStopCycles,
            stopDuration.getOrElse(""),
            action.commitAppliedCycle.get,
            action.commitAppliedCycle.get - action.startCycle,
            action.finishCycle - action.commitAppliedCycle.get,
            action.finishCycle,
            action.resumeCycle.map(_.toString).getOrElse(""),
            cleanup.map(_._2.startCycle.toString).getOrElse(""),
            cleanup.flatMap(_._2.commitCycle).map(_.toString).getOrElse(""),
            cleanup.flatMap(_._2.commitAppliedCycle).map(_.toString).getOrElse(""),
            cleanup.map(_._2.finishCycle.toString).getOrElse(""),
            cleanup.map(_._1.instructions.size.toString).getOrElse(""),
            cleanup.map { case (_, a) => (a.commitAppliedCycle.get - a.startCycle).toString }.getOrElse(""),
            cleanup.map { case (_, a) => (a.finishCycle - a.commitAppliedCycle.get).toString }.getOrElse(""),
            transaction.cleanupOf.getOrElse(""),
            action.prefilledTokens.map(_.toString).getOrElse("")
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
