package rio.sim

import spinal.core.sim._
import rio._

object WeightedRankerSim extends App {
  val config = EngineConfig(2, 8, 65536, 8, 4)
  SimConfig.withIVerilog.addSimulatorFlag("-g2012").compile(WeightedRanker(config)).doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.io.accept #= false
    dut.io.pop.valid #= false
    dut.io.control.valid #= false
    dut.io.commit #= false
    dut.io.port #= 1
    dut.io.flow #= 1
    dut.clockDomain.waitSampling(5)
    def command(kind: ControlCommand.E, flow: Int, data: Int): Unit = {
      dut.io.control.valid #= true
      dut.io.control.command #= kind
      dut.io.control.engineId #= 1
      dut.io.control.vPifoId #= 1
      dut.io.control.flowId #= flow
      dut.io.control.data #= data
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitFallingEdge()
      dut.io.control.valid #= false
    }
    def commit(): Unit = {
      dut.io.commit #= true
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitFallingEdge()
      dut.io.commit #= false
    }
    def push(flow: Int, expected: Int): Unit = {
      dut.io.flow #= flow
      dut.io.accept #= true
      sleep(1)
      assert(dut.io.rank.toInt == expected, s"flow=$flow expected=$expected got=${dut.io.rank.toInt}")
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitFallingEdge()
      dut.io.accept #= false
    }
    command(ControlCommand.UpdateRankGroup, 1, 1)
    command(ControlCommand.UpdateRankGroup, 2, 1) // two flows share one tenant finish
    command(ControlCommand.UpdateRankQuantum, 1, 40)
    commit()
    push(1, 40); push(2, 80); push(1, 120)
    command(ControlCommand.UpdateRankQuantum, 1, 20)
    push(2, 160) // staged reweight must not apply before commit
    commit()
    push(1, 180); push(2, 200) // no reset of finish, no stale-read hazard
    println("[WeightedRankerSim] grouped finish, atomic reweight and consecutive admissions passed")
  }
}
