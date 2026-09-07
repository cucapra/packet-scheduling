package rio

/** Evaluation storage only: occupancy probes and a separate frozen-state copy/inject path. */
class EvaluationPifoRTL(config: PifoConfig) extends ConcurrentPifoRTL(config, evaluation = true)
