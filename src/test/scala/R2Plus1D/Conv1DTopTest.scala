package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._
import R2Plus1D.model.ConvType
import R2Plus1D.model.ConvConfig
import R2Plus1D.model.Conv0D

class Conv1DTopTest extends org.scalatest.flatspec.AnyFlatSpec {

  "Conv0D" should "work right in loopCtrl1D " in {
    val config = ConvConfig(Uic = 2, Uoc = 2, Nic = 3, Noc = 5, Nid = 2, Nihw = 4, stride = 2, padding = 0, K = 1, convType = ConvType.D0)
    config.display()
    val conv0D    = Conv0D(config)
    val ifMap     = conv0D.randomIfMap
    val weight    = conv0D.randomWeight
    val ifMapMem  = conv0D.ifMap2Mem(ifMap)
    val weightMem = conv0D.weight2Mem(weight)
    conv0D.loopUnroll(ifMapMem, weightMem)

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv1DTop(uc = config.Uoc, uoc = config.Uoc)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.ifMapRdy   #= false
        io.weightRdy  #= false
        io.loadConfig #= false
        io.shortCut   #= true
        io.configPorts.assignConfig(config)
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        io.weightRdy  #= true
        clockDomain.waitSampling(10)
        io.ifMapRdy #= true
        clockDomain.waitSampling(200)
      }

  }

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
        val dut = Conv1DTop(uc = config.Uic, uoc = config.Uoc, dataWidth = 4)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.loadConfig #= false
        io.weightRdy  #= false
        io.ifMapRdy   #= false
        io.shortCut   #= false
        io.configPorts.assignConfig(config)
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        io.weightRdy  #= true
        clockDomain.waitSampling(15)
        io.ifMapRdy #= true
        clockDomain.waitSampling(1000)
      }

  }
}
