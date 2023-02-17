package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw.xilinx._
import R2Plus1D.model.{ConvConfig, ConvType}
import org.scalatest.flatspec.AnyFlatSpec

class Conv2P1Test extends AnyFlatSpec {

  it should " work right" in {

    val config2D = ConvConfig(convType = ConvType.D2, Uic = 4, Nic = 5, Uoc = 16, Noc = 18, Nihw = 6, Nid = 4, stride = 1, K = 3, padding = 1)
    val config1D =
      ConvConfig(
        convType = ConvType.D1,
        Uic      = config2D.Uoc,
        Nic      = config2D.Noc,
        Uoc      = config2D.Uic,
        Noc      = 5,
        Nihw     = config2D.Nohw,
        Nid      = config2D.Nod,
        K        = 3,
        stride   = 2,
        padding  = 1
      )

    println("-" * 20 + "2D config" + "-" * 20)
    config2D.display()

    val conv2D   = model.Conv2D(config2D)
    val ifMap2D  = conv2D.randIfMap()
    val weight2D = conv2D.randWeight()
    conv2D.loopUnroll(conv2D.ifMap2Mem(ifMap2D), conv2D.weight2Mem(weight2D), false)

    println("-" * 20 + "1D config" + "-" * 20)
    config1D.display()

    val conv1D      = model.Conv1D(config1D)
    val ifMap1D     = conv1D.randomIfMap
    val weight1D    = conv1D.randomWeight
    val ifMapMem1D  = conv1D.ifMap2Tile(ifMap1D)
    val weightMem1D = conv1D.weight2Tile(weight1D)
    conv1D.loopUnroll(ifMapMem1D, weightMem1D)

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
        val dut = Conv2P1(uic = config2D.Uic, uc = config2D.Uoc, uoc = config1D.Uoc)
        dut.loopCtrl2D.io.ofMapBuffer2DRdy.simPublic()
        dut
      }
      .doSim { dut =>
        import dut._
        println(dut.PE2D.PELatency)
        dut.clockDomain.forkStimulus(10)
        SimTimeout(11000 * 10)
        io.config2D.assignConfig(config2D)
        io.config1D.assignConfig(config1D)
        io.shortCut              #= false
        io.weight2DRdy           #= false
        io.weight1DRdy           #= false
        io.fMapDDRLoadDone       #= false
        io.newConv               #= false
        io.load1DResInto0DBuffer #= true
        io.addition              #= false
        clockDomain.waitSampling()
        io.newConv         #= true
        io.weight1DRdy     #= true
        io.weight2DRdy     #= true
        io.fMapDDRLoadDone #= true
        clockDomain.waitSampling()
        io.newConv #= false
        clockDomain.waitSampling(10000)
      }

  }
}
