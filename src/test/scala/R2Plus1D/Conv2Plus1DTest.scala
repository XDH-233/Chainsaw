package R2Plus1D

import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw.xilinx._
import R2Plus1D.model.{ConvConfig, ConvType}

class Conv2Plus1DTest extends org.scalatest.flatspec.AnyFlatSpec {

  "patten4: Block2 no 0d in this Block" should "work right" in {
    val config2D = ConvConfig(convType = ConvType.D2, Uic = 4, Nic = 5, Uoc = 16, Noc = 18, Nihw = 6, Nid = 4, stride = 1, K = 3, padding = 1)
    val config1D =
      ConvConfig(
        convType = ConvType.D1,
        Uic      = config2D.Uoc,
        Nic      = 18,
        Uoc      = config2D.Uic,
        Noc      = 5,
        Nihw     = config2D.Nohw,
        Nid      = config2D.Nod,
        K        = 3,
        stride   = 2,
        padding  = 1
      )
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv2Plus1D(uic = config2D.Uic, uc = config2D.Uoc, uoc = config1D.Uoc)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.shortCut              #= true
        io.weight2DRdy           #= false
        io.weight1DRdy           #= false
        io.fMapRdy               #= false
        io.load2DConfig          #= false
        io.load1DConfig          #= false
        io.load1DResTo0DBuffer   #= true // 0D result
        io.elementWiseAdditionEn #= false
        io.addition2To0DBBuffer  #= false
        clockDomain.waitSampling()
      }

  }

  "patten3: Block1 and 0D" should "work right " in {
    val config2D = ConvConfig(convType = ConvType.D2, Uic = 4, Nic = 5, Uoc = 16, Noc = 18, Nihw = 6, Nid = 4, stride = 1, K = 3, padding = 1)
    val config1D =
      ConvConfig(convType = ConvType.D1, Uic = config2D.Uoc, Nic = 18, Uoc = config2D.Uic, Noc = 5, Nihw = 6, Nid = 4, K = 3, stride = 2, padding = 1)
    val config0D = ConvConfig(
      convType = ConvType.D0,
      Uic      = config2D.Uic,
      Uoc      = config1D.Uoc,
      Nic      = config2D.Nic,
      Noc      = 9,
      Nihw     = config2D.Nihw,
      Nid      = config2D.Nid,
      K        = 1,
      stride   = 2,
      padding  = 0
    )
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv2Plus1D(uic = config2D.Uic, uc = config2D.Uoc, uoc = config1D.Uoc)
        dut.loopCtrl2D.io.ofMapBuffer2DRdy.simPublic()
        dut
      }
      .doSimUntilVoid { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        SimTimeout(11000 * 10)
        io.configPorts2D.assignConfig(config2D)
        io.configPorts1D.assignConfig(config0D)
        io.shortCut              #= true
        io.weight2DRdy           #= false
        io.weight1DRdy           #= false
        io.fMapRdy               #= false
        io.load2DConfig          #= false
        io.load1DConfig          #= false
        io.load1DResTo0DBuffer   #= true // 0D result
        io.elementWiseAdditionEn #= false
        io.addition2To0DBBuffer  #= false
        clockDomain.waitSampling()

        val conv2DThread = fork {
          io.load2DConfig #= true
          io.load1DConfig #= true
          clockDomain.waitSampling()
          io.load2DConfig #= false
          io.load1DConfig #= false
          io.weight2DRdy  #= true
          io.weight1DRdy  #= true
          clockDomain.waitSampling(10)
          io.fMapRdy #= true
          clockDomain.waitSampling(6000)
        }

        val ofBufferRdyMonitor = fork {
          var loadDone = false
          (0 until 10000).foreach { _ =>
            clockDomain.waitSampling()
            if (dut.loopCtrl2D.io.ofMapBuffer2DRdy.toBoolean && !loadDone) {
              loadDone = true
              io.configPorts1D.assignConfig(config0D)
              io.shortCut #= false
              clockDomain.waitSampling()
              io.load1DConfig #= true
              clockDomain.waitSampling()
              io.load1DConfig #= false
              clockDomain.waitSampling()

            }

          }
          simSuccess()
        }
      }
  }

  "patten1: stem (2+1)D and  patten2: Bock1, no 0D" should "work right" in {
    val config2D = ConvConfig(convType = ConvType.D2, Uic = 4, Nic = 5, Uoc = 16, Noc = 18, Nihw = 6, Nid = 4, stride = 1, K = 3, padding = 1)
    val config1D = ConvConfig(convType = ConvType.D1, Uic = 16, Nic = 18, Uoc = 4, Noc = 5, Nihw = 6, Nid = 4, K = 3, stride = 2, padding = 1)

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv2Plus1D(uic = config2D.Uic, uc = config2D.Uoc, uoc = config1D.Uoc)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.configPorts2D.assignConfig(config2D)
        io.configPorts1D.assignConfig(config1D)
        io.shortCut              #= false
        io.weight2DRdy           #= false
        io.weight1DRdy           #= false
        io.fMapRdy               #= false
        io.load2DConfig          #= false
        io.load1DConfig          #= false
        io.load1DResTo0DBuffer   #= true // patten1 -> true, patten2 -> false
        io.elementWiseAdditionEn #= false
        io.addition2To0DBBuffer  #= false
        clockDomain.waitSampling()
        io.load2DConfig #= true
        io.load1DConfig #= true
        clockDomain.waitSampling()
        io.load2DConfig #= false
        io.load1DConfig #= false
        io.weight2DRdy  #= true
        io.weight1DRdy  #= true
        clockDomain.waitSampling(10)
        io.fMapRdy #= true
        clockDomain.waitSampling(6000)
      }

  }

}
