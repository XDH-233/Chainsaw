package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class LoopCtrl2DTest extends org.scalatest.flatspec.AnyFlatSpec {

  "loopCtrl2D" should "run in high frequency" in MyVivadoAction(LoopCtrl2D(), "loop_ctrl_2D", SYNTH)

  it should "work right" in {
    // -------------------Conv2D model----------------------------------------------------------------------------------
    val config =
      model.Conv2DConfig(Uic = 4, Uc = 12, Nic = 9, Nc = 16, Nd = 2, Nihw = 5, Krs = 3, stride = 2, padding = 2)
    val conv2D = model.Conv2D(config)
    config.display()
    val ifMap  = conv2D.randIfMap()
    val weight = conv2D.randWeight()
    conv2D.loopUnroll(conv2D.ifMap2Mem(ifMap), conv2D.weight2Mem(weight))
    // -------------------Simulation------------------------------------------------------------------------------------
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = LoopCtrl2D(uic = config.Uic, uc = config.Uc)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        import io.configParaPorts._
        Nic           #= config.Nic // Uic = 36
        Nc            #= config.Nc // Uc = 144
        Nd            #= config.Nd
        Nohw          #= config.Nohw
        Krs           #= config.Krs
        Toh           #= config.Toh
        Tow           #= config.Tow
        io.loadConfig #= false
        NcDUcCeil     #= config.NcDUcCeil
        NicDUicCeil   #= config.NicDUicCeil
        NohwDTohCei   #= config.NohwDTohCeil
        NohwDTowCei   #= config.NohwDTowCeil
        kernelSize    #= config.kernelSize
        Nihw          #= config.Nihw
        stride        #= config.stride > 1
        padding       #= config.padding
        ifMapSize     #= config.ifMapSize
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        clockDomain.waitSampling()
        clockDomain.waitSampling(2000)
      }

  }
}
