package rio.sim

/** Explicit opt-in build of the maintenance-capable evaluation mesh. */
object EvaluationRequestSimulatorCli {
  def main(args: Array[String]): Unit = RequestSimulatorCli.run(args, evaluation = true)
}
