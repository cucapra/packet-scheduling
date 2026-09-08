package rio.sim

import rio._

/** Reject an oversized retained epoch before it can block its own commit. */
object ReplayEpochCapacity {
  def advance(config: EngineConfig, retained: Int, command: ControlCommand.E): Int = {
    if (!config.dynamicConfig || config.mapperSync != "replay" || command == ControlCommand.CommitMapper) return 0
    val mapper = command == ControlCommand.UpdateMapperPre || command == ControlCommand.UpdateMapperPost
    val next = if (mapper || retained > 0) retained + 1 else 0
    require(next < config.commitQueueLength,
      s"replay epoch exceeds ${config.commitQueueLength - 1} retained commands; " +
        "increase --control-queue-depth (EngineConfig.commitQueueLength) or end the epoch with CommitMapper")
    next
  }

  def validate(config: EngineConfig, commands: Iterable[ControlCommand.E]): Unit = {
    commands.foldLeft(0)((retained, command) => advance(config, retained, command))
  }
}
