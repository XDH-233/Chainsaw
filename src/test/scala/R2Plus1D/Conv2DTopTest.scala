package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class Conv2DTopTest extends org.scalatest.flatspec.AnyFlatSpec {

  "Conv2DTop" should "work in high frequency" in MyVivadoAction(design = Conv2DTop(dataWidth = 8, uic = 36, uc = 144), name = "conv_2D_top", flowType = SYNTH)

  it should "work right" in {
    // -------------------Conv2D model----------------------------------------------------------------------------------
    val config =
      model.Conv2DConfig(Uic = 4, Uc = 12, Nic = 9, Nc = 16, Nd = 2, Nihw = 5, Krs = 3, stride = 2, padding = 2)
    val conv2D = model.Conv2D(config)
    config.display()
    val ifMap  = conv2D.randIfMap()
    val weight = conv2D.randWeight()
    conv2D.loopUnroll(conv2D.ifMap2Mem(ifMap), conv2D.weight2Mem(weight))
    // ------------------- simulation ----------------------------------------------------------------------------------
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv2DTop(uic = config.Uic, uc = config.Uc, dataWidth = 8)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        import io.configParaPorts._
        io.configParaPorts.assignConfig(config)
        io.loadConfig      #= false
        ifMapSize          #= config.ifMapSize
        io.weightBufferRdy #= false
        io.ifMapRdy        #= false

        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        clockDomain.waitSampling()
        io.weightBufferRdy #= true
        clockDomain.waitSampling(15)
        io.ifMapRdy #= true
        clockDomain.waitSampling(3000)
      }

  }

}
