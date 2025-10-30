package rio.sim

import spinal.core._
import spinal.core.sim._

object SimUtils {
    def RioSimConfig = SimConfig.withIVerilog
      .addSimulatorFlag("-g2012")
      .withFstWave
}