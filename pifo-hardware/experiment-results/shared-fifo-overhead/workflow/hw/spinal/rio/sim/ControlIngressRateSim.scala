package rio.sim

import scala.collection.mutable.ArrayBuffer

import spinal.core.sim._

import rio._

/** Check that a transaction package cannot place two instructions on the one
  * mesh control ingress in the same clock cycle.
  *
  * Run from pifo-hardware with:
  *   sbt "runMain rio.sim.ControlIngressRateSim"
  */
object ControlIngressRateSim extends App {
  private val clockPeriod = 10L
  private val config = EngineConfig(
    numEngines = 1,
    numVPIFOs = 4,
    maxPacketPriority = 32,
    fifoDepth = 2,
    prefetchBufferDepth = 2
  )

  SimConfig.withIVerilog
    .addSimulatorFlag("-g2012")
    .compile(PifoMesh(config))
    .doSim { dut =>
      val controller = PifoMeshSimController(config, dut)
      controller.start

      val acceptanceTimes = ArrayBuffer.empty[Long]
      val packageCommands = Vector(
        (ControlCommand.UpdateBrainEngine, 0, 3),
        (ControlCommand.UpdateBrainEngine, 1, 3),
        (ControlCommand.UpdateBrainEngine, 2, 3),
        (ControlCommand.UpdateMapperPre, 1, 2),
        (ControlCommand.UpdateMapperNonExist, 2, 3),
        (ControlCommand.CommitMapper, 0, 0)
      )

      packageCommands.foreach { case (command, vPifoId, data) =>
        controller.sendControl(
          command,
          engineId = 1,
          data = data,
          vPifoId = vPifoId,
          onAccepted = () => acceptanceTimes += simTime()
        )
      }

      assert(acceptanceTimes.size == packageCommands.size)
      val gaps = acceptanceTimes.sliding(2).map(pair => pair(1) - pair(0)).toVector
      assert(
        gaps.forall(_ >= clockPeriod),
        s"control instructions were accepted more than once per cycle: times=${acceptanceTimes.mkString(",")}"
      )
      assert(
        gaps.contains(clockPeriod),
        s"control ingress did not demonstrate one-instruction-per-cycle throughput: gaps=${gaps.mkString(",")}"
      )
      println(
        s"[ControlIngressRateSim] accepted=${acceptanceTimes.size} " +
          s"acceptanceTimes=${acceptanceTimes.mkString(",")} gaps=${gaps.mkString(",")}"
      )

      simSuccess()
    }
}
