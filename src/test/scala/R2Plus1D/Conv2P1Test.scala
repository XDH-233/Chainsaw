package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw.xilinx._
import Chainsaw._
import R2Plus1D.model.{Conv0D, Conv1D, Conv2D, ConvConfig, ConvType}
import org.scalatest.flatspec.AnyFlatSpec

class Conv2P1Test extends AnyFlatSpec {

  " conv2p1 " should "run in high frequency" in MyVivadoAction(Conv2P1(), "conv_2_plus_1", SYNTH)

  "new test " should "run in high freq" in VivadoSynth(Conv2P1(), "conv_2_plus_1")

  val config2D: ConvConfig = ConvConfig(convType = ConvType.D2, Uic = 4, Nic = 5, Uoc = 16, Noc = 18, Nihw = 6, Nid = 4, stride = 1, K = 3, padding = 1)
  val config1D: ConvConfig =
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

  val config0D: ConvConfig = ConvConfig(
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
  val conv2D:      Conv2D            = model.Conv2D(config2D)
  val ifMapMem2D:  Array[Array[Int]] = conv2D.ifMap2Mem(conv2D.randIfMap())
  val weightMem2D: Array[Array[Int]] = conv2D.weight2Mem(conv2D.randWeight())

  val conv1D:      Conv1D            = model.Conv1D(config1D)
  val ifMapMem1D:  Array[Array[Int]] = conv1D.ifMap2Tile(conv1D.randomIfMap)
  val weightMem1D: Array[Array[Int]] = conv1D.weight2Tile(conv1D.randomWeight)

  val conv0D:      Conv0D            = model.Conv0D(config0D)
  val ifMapMem0D:  Array[Array[Int]] = conv0D.ifMap2Mem(conv0D.randomIfMap)
  val weightMem0D: Array[Array[Int]] = conv0D.weight2Mem(conv0D.randomWeight)

  "patten3: Block0 1*1*1" should "work right" in pattenSim(
    addition         = false,
    shorCut          = true,
    load1DTo0DBuffer = false,
    print2D          = false,
    print1D          = false,
    print0D          = true
  )

  "patten2: Block0, no 1*1*1" should "work right" in pattenSim(
    addition         = false,
    shorCut          = false,
    load1DTo0DBuffer = false,
    print2D          = false,
    print1D          = true,
    print0D          = false
  )

  "patten1: stem conv" should "work right " in pattenSim(
    addition         = false,
    load1DTo0DBuffer = true,
    shorCut          = false,
    print2D          = false,
    print1D          = true,
    print0D          = false
  )

  def pattenSim(shorCut: Boolean, addition: Boolean, load1DTo0DBuffer: Boolean, print2D: Boolean, print1D: Boolean, print0D: Boolean): Unit = {
    println("-" * 20 + "2D config" + "-" * 20)
    config2D.display()
    println("-" * 20 + "0D config" + "-" * 20)
    config0D.display()
    println("-" * 20 + "1D config" + "-" * 20)
    config1D.display()
    conv2D.loopUnroll(ifMapMem2D, weightMem2D, print2D)
    conv1D.loopUnroll(ifMapMem1D, weightMem1D, print1D)
    conv0D.loopUnroll(ifMapMem0D, weightMem0D, print0D)

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv2P1(uic = config2D.Uic, uc = config2D.Uoc, uoc = config1D.Uoc)
        dut
      }
      .doSim { dut =>
        println(dut.PE1D.PELatency)
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.config2D.assignConfig(config2D)
        io.config1D.assignConfig(config1D)
        io.config0D.assignConfig(config0D)
        io.shortCut              #= shorCut
        io.weight2DRdy           #= false
        io.weight1DRdy           #= false
        io.fMapDDRLoadDone       #= false
        io.newConv               #= false
        io.load1DResInto0DBuffer #= load1DTo0DBuffer
        io.addition              #= addition
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
