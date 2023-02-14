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
    val config = model.ConvConfig(Uc = 2, Uoc = 6, Nc = 3, Noc = 8, Nid = 5, Nihw = 3, K = 3, stride = 2, padding = 1, convType = ConvType.D1)
    config.display()
    val conv1D = model.Conv1D(config)
    val ifMap  = conv1D.randomIfMap
    val weight = conv1D.randomWeight
    conv1D.loopUnroll(conv1D.ifMap2Tile(ifMap), conv1D.weight2Tile(weight))
    // ----------------------- simulation -------------------------------------------------------------------------------
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = LoopCtrl1D(uc = config.Uc, uoc = config.Uoc, readLatencyURAM = 4, readLatencyBRAM = 2, PELatency = 3)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.loadConfig #= false

        io.config.Nihw        #= config.Nihw
        io.config.Kt          #= config.K
        io.config.Nid         #= config.Nid
        io.config.Nod         #= config.Nod
        io.config.Nc          #= config.Nc
        io.config.Noc         #= config.Noc
        io.config.NcDUcCeil   #= config.NcDUcCeil
        io.config.NocDUocCeil #= config.NocDUocCeil
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        clockDomain.waitSampling(1000)
      }
  }
}
