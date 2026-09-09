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
    Config.spinal.copy(targetDirectory = work.toString).generateVerilog(ReplayControlFifo(config))
    val rtl = work.resolve("ReplayControlFifo.v")
    val body = Files.readString(rtl)
    val arrays = """\breg\s+\[(\d+):0\]\s+(\w+)\s+\[0:(\d+)\]""".r
      .findAllMatchIn(body).map(m => (m.group(1), m.group(2), m.group(3))).toVector
    assert(arrays == Vector(("44", "storage", (depth - 1).toString)), arrays)
    assert("""\bstorage\[[^\]]+\]\s*<=""".r.findAllIn(body).size == 1)
    assert("""<=\s*storage\[""".r.findAllIn(body).size == 1)
    assert("""always @\(posedge clk\) begin\s+if\(readEnable\) begin\s+\w+ <= storage\[""".r.findFirstIn(body).nonEmpty)
    val executable = work.resolve("fifo.vvp")
    val compile = Seq("iverilog", "-g2012", s"-DCONTROL_DEPTH=$depth", "-s", "shared_control_fifo_tb",
      "-o", executable.toString, rtl.toString, "hw/verilog/shared_control_fifo_tb.sv")
    require(Process(compile).! == 0, "FIFO test compilation failed")
    val log = Process(Seq("vvp", executable.toString)).!!
    Files.writeString(work.resolve("simulate.log"), log)
    print(log)
    assert(log.contains(s"SHARED_FIFO_PASS depth=$depth ") && !log.contains("FIFO_FAIL"), log)
  }
}
