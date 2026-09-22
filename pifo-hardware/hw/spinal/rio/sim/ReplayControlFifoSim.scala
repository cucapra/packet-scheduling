package rio.sim

import java.nio.file.{Files, Paths}
import scala.sys.process._

import rio._

/** Independent command-order scoreboard, including the default 256-entry RAM.
  * Requires Icarus Verilog on PATH. Generates/simulates RTL only.
  */
object ReplayControlFifoSim extends App {
  val depths = Seq(2, 4, 8, EngineConfig.DefaultCommitQueueLength).distinct
  for (depth <- depths) {
    val work = Paths.get("simWorkspace", s"ReplayControlFifo-$depth").toAbsolutePath
    Files.createDirectories(work)
    val config = EngineConfig(2, 8, 256, 4, 2, commitQueueLength = depth)
    val report = Config.spinal.copy(targetDirectory = work.toString).generateVerilog(ReplayControlFifo(config))
    val rtl = work.resolve("ReplayControlFifo.v")
    val commandWidth = report.toplevel.io.push.command.getBitsWidth
    val executable = work.resolve("fifo.vvp")
    val compile = Seq("iverilog", "-g2012", s"-DCONTROL_DEPTH=$depth", s"-DCOMMAND_WIDTH=$commandWidth",
      "-s", "shared_control_fifo_tb",
      "-o", executable.toString, rtl.toString, "hw/verilog/shared_control_fifo_tb.sv")
    require(Process(compile).! == 0, "FIFO test compilation failed")
    val log = Process(Seq("vvp", executable.toString)).!!
    Files.writeString(work.resolve("simulate.log"), log)
    print(log)
    assert(log.contains(s"SHARED_FIFO_PASS depth=$depth ") && !log.contains("FIFO_FAIL"), log)
  }
}
