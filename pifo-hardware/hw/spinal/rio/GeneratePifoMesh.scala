package rio

import spinal.core._
import spinal.lib._

/** Elaborate the complete existing hardware with all runtime ports exposed.
  * No simulation controller, stimulus, or constant configuration is included.
  */
object GeneratePifoMesh {
  def main(args: Array[String]): Unit = {
    require(args.length >= 5 && args.length <= 8,
      "Usage: GeneratePifoMesh OUTPUT_DIR NUM_ENGINES NUM_VPIFOS FIFO_DEPTH PRIORITY_BITS [house|external] [replay|static] [CONTROL_QUEUE_DEPTH]")
    val Array(output, enginesArg, vpifosArg, depthArg, priorityArg) = args.take(5)
    val backend = args.lift(5).getOrElse("house")
    val configuration = args.lift(6).getOrElse("replay")
    require(Set("static", "replay").contains(configuration))
    val engines = enginesArg.toInt
    val vpifos = vpifosArg.toInt
    val depth = depthArg.toInt
    val priorityBits = priorityArg.toInt
    require(engines > 0, "NUM_ENGINES must be positive")
    require(vpifos >= 2 && (vpifos & (vpifos - 1)) == 0,
      "NUM_VPIFOS must be a power of two >= 2 (also the current global flow-ID capacity)")
    require(depth > 0 && vpifos.toLong * depth <= Int.MaxValue, "invalid FIFO_DEPTH")
    require(priorityBits >= 5 && priorityBits <= 30, "PRIORITY_BITS must be in [5, 30]")

    val config = EngineConfig(
      numEngines = engines,
      numVPIFOs = vpifos,
      maxPacketPriority = 1 << priorityBits,
      fifoDepth = depth,
      prefetchBufferDepth = 2,
      pifoBackend = backend,
      dynamicConfig = configuration != "static",
      commitQueueLength = args.lift(7).map(_.toInt).getOrElse(EngineConfig.DefaultCommitQueueLength)
    )
    Config.spinal.copy(targetDirectory = output).generateVerilog(PifoMesh(config))
    println(s"PifoMesh: $engines PEs, $vpifos vPIFO IDs/PE, " +
      s"${vpifos * depth} shared entries/PE, $vpifos global flow IDs, $priorityBits rank bits, $backend PIFO, $configuration configuration")
  }
}

/** Measure one matching house PIFO, including its accepted-push notifications. */
object GeneratePifoComponent {
  def main(args: Array[String]): Unit = {
    val Array(output, engines, flows, depth, rank) = args
    val config = EngineConfig(numEngines = engines.toInt, numVPIFOs = flows.toInt,
      fifoDepth = depth.toInt, maxPacketPriority = 1 << rank.toInt, prefetchBufferDepth = 2)
    Config.spinal.copy(targetDirectory = output).generateVerilog(new Component {
      setDefinitionName("PifoMesh")
      val io = slave(PifoBoundary(config))
      val core = new ConcurrentPifoRTL(config)
      core.io.push1 << io.push1
      core.io.push2.valid := False
      core.io.push2.payload.assignFromBits(B(0, core.io.push2.payload.getBitsWidth bits))
      core.io.popRequest << io.popRequest
      io.popResponse << core.io.popResponse
      io.popPortEmpty := core.io.popPortEmpty
      io.portDrained << core.io.portDrained
      (io.portPushed zip core.io.portPushed).foreach { case (to, from) => to << from }
    })
  }
}
