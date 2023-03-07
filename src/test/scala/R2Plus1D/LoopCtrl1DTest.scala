package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw.xilinx._
import scala.tools.nsc.interactive.ContextTrees
import R2Plus1D.model.ConvType

class LoopCtrl1DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right " in {
    // ----------------------- model ------------------------------------------------------------------------------------
    val config = model.ConvConfig(Uic = 2, Uoc = 6, Nic = 3, Noc = 8, Nid = 5, Nihw = 3, K = 3, stride = 2, padding = 1, convType = ConvType.D1)
    config.display()
    val conv1D = model.Conv1D(config)
    val ifMap  = conv1D.randomIfMap
    val weight = conv1D.randomWeight
    conv1D.loopUnroll(conv1D.ifMap2Mem(ifMap), conv1D.weight2Mem(weight))
    // ----------------------- simulation -------------------------------------------------------------------------------
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = LoopCtrl1D(uc = config.Uic, uoc = config.Uoc, readLatencyURAM = 4, readLatencyBRAM = 2, PELatency = 3)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.loadConfig #= false
        io.config.assignConfig(config)

        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        clockDomain.waitSampling(1000)
      }
  }
}
