package rio

/** Evaluation PE only: token prefill, copy ports and the hierarchical weighted ranker. */
class EvaluationPifoEngine(config: EngineConfig) extends PifoEngine(config, evaluation = true)
