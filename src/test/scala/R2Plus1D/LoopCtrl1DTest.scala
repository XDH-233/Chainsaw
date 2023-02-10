package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw.xilinx._

class LoopCtrl1DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right " in {
    // ----------------------- model ------------------------------------------------------------------------------------
    val config = model.Conv1DConfig(Uc = 2, Uoc = 6, Nc = 3, Noc = 8, Nid = 5, Nhw = 3, Kt = 3, stride = 2, padding = 1)
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
        val dut = LoopCtrl1D(uc = config.Uc, uoc = config.Uoc)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.loadConfig #= false
        io.PEDone     #= false

        io.Nhw         #= config.Nhw
        io.Kt          #= config.Kt
        io.Nid         #= config.Nid
        io.Nod         #= config.Nod
        io.Nc          #= config.Nc
        io.Noc         #= config.Noc
        io.NcDUcCeil   #= config.NcDUcCeil
        io.NocDUocCeil #= config.NocDUocCeil
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        io.PEDone     #= true
        clockDomain.waitSampling(1000)
      }
  }
}
