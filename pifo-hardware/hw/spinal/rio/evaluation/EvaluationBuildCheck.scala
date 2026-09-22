package rio

import java.nio.file.{Files, Paths}

/** Elaboration-only isolation check: no RTL regression simulation is needed. */
object EvaluationBuildCheck extends App {
  for (command <- ControlCommand.elements) {
    rio.sim.SimUtils.requireSupportedCommand(command, 0, evaluation = true)
    val rejected = scala.util.Try(rio.sim.SimUtils.requireSupportedCommand(command, 0, evaluation = false)).isFailure
    assert(rejected == (command.position >= ControlCommand.StopWorld.position), s"command isolation: $command")
  }
  assert(scala.util.Try(rio.sim.SimUtils.requireSupportedCommand(
    ControlCommand.UpdateBrainEngine, 4, evaluation = false)).isFailure)
  val root = Paths.get(args.headOption.getOrElse("simWorkspace/evaluation-build-check"))
  val config = EngineConfig(2, 4, 256, 4, 2)
  for (evaluation <- Seq(false, true)) {
    val directory = root.resolve(if (evaluation) "evaluation" else "production")
    val report = Config.spinal.copy(targetDirectory = directory.toString).generateVerilog {
      if (evaluation) new EvaluationPifoMesh(config) else PifoMesh(config)
    }
    val rtl = report.generatedSourcesPaths.map(path => Files.readString(Paths.get(path))).mkString("\n")
    for (feature <- Seq("PifoCopyController", "prefill_remaining", "trafficStopped", "io_copyInsert_valid",
        "io_inspectPifo", "io_maintenanceIdle", "WeightedRanker", "priority_encode_log_evaluation")) {
      assert(rtl.contains(feature) == evaluation, s"isolation failure: $feature in $directory")
    }
    assert(rtl.contains("ReplayControlFifo"), "both images must use shared-FIFO mapper replay")
    println(s"BUILD PASS: $directory evaluation=$evaluation")
  }
}
