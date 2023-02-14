package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._
import R2Plus1D.model.ConvType

class LoopCtrl2DTest extends org.scalatest.flatspec.AnyFlatSpec {

  "loopCtrl2D" should "run in high frequency" in MyVivadoAction(LoopCtrl2D(), "loop_ctrl_2D", SYNTH)

  it should "work right" in {
    // -------------------Conv2D model----------------------------------------------------------------------------------
    val config =
      model.ConvConfig(Uic = 4, Uc = 12, Nic = 9, Nc = 16, Nid = 2, Nihw = 5, K = 3, stride = 2, padding = 2, convType = ConvType.D2)
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
        val dut = LoopCtrl2D(uic = config.Uic, uc = config.Uc, PELatency = 4)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.config.assignConfig(config)
        io.loadConfig #= false
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        clockDomain.waitSampling()
        clockDomain.waitSampling(2000)
      }

  }
}
