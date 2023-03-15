package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw.xilinx._
import Chainsaw._
import R2Plus1D.model.{Conv0D, Conv1D, Conv2D, ConvConfig, ConvType}
import org.scalatest.flatspec.AnyFlatSpec

class Conv2P1Test extends AnyFlatSpec {

  "conv2p1 place" should "run in high frequency" in MyVivadoAction(Conv2P1(), "conv2p1_place", IMPL)

  " conv2p1 synth" should "run in high frequency" in MyVivadoAction(Conv2P1(), "conv_2_plus_1", SYNTH)

  val config2D: ConvConfig = ConvConfig(Uic = 4, Uoc = 8, Nic = 9, Tc = 16, Noc = 18, Nid = 2, Nihw = 5, K = 2, stride = 1, padding = 1, convType = ConvType.D2)
  val config1D: ConvConfig =
    ConvConfig(
      convType = ConvType.D1,
      Uic      = config2D.Uoc,
      Nic      = config2D.Noc,
      Uoc      = config2D.Uic,
      Noc      = 18,
      Nihw     = config2D.Nohw,
      Nid      = config2D.Nod,
      Tc       = 8,
      K        = 3,
      stride   = 2,
      padding  = 1
    )

  val config0D: ConvConfig = ConvConfig(
    convType = ConvType.D0,
    Uic      = config2D.Uic,
    Uoc      = config1D.Uoc,
    Nic      = config2D.Nic,
    Tc       = 8,
    Noc      = 8,
    Nihw     = config2D.Nihw,
    Nid      = config2D.Nid,
    K        = 1,
    stride   = 2,
    padding  = 0
  )
  val conv2D:      Conv2D                   = model.Conv2D(config2D)
  val ifMapMem2D:  Array[Array[Int]]        = conv2D.ifMap2Mem(conv2D.randIfMap())
  val weightMem2D: Array[Array[Array[Int]]] = conv2D.weight2Mem(conv2D.randWeight())

  val conv1D:      Conv1D                   = model.Conv1D(config1D)
  val ifMapMem1D:  Array[Array[Int]]        = conv1D.ifMap2Mem(conv1D.randomIfMap)
  val weightMem1D: Array[Array[Array[Int]]] = conv1D.weight2Mem(conv1D.randomWeight)

  val conv0D:      Conv0D                   = model.Conv0D(config0D)
  val ifMapMem0D:  Array[Array[Int]]        = conv0D.ifMap2Mem(conv0D.randomIfMap)
  val weightMem0D: Array[Array[Array[Int]]] = conv0D.weight2Mem(conv0D.randomWeight)

  "patten3: Block0 1*1*1" should "work right" in pattenSim(
    addition = false,
    shorCut  = true,
    stem     = false,
    noNext0D = false,
    print2D  = false,
    print1D  = false,
    print0D  = true
  )

  "patten2: Block0, no 1*1*1" should "work right" in pattenSim(
    addition = false,
    shorCut  = false,
    stem     = false,
    noNext0D = false,
    print2D  = false,
    print1D  = true,
    print0D  = false
  )

  "patten1: stem conv" should "work right " in pattenSim(
    addition = false,
    stem     = true,
    shorCut  = false,
    noNext0D = false,
    print2D  = true,
    print1D  = false,
    print0D  = false
  )

  def pattenSim(shorCut: Boolean, addition: Boolean, stem: Boolean, noNext0D: Boolean, print2D: Boolean, print1D: Boolean, print0D: Boolean): Unit = {
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
        dut.loopCtrl2D.io.ifMapAddr.simPublic()
        dut.loopCtrl2D.io.ofMapAddr.simPublic()
        dut.loopCtrl2D.io.weightAddrBase.simPublic()
        dut.loopCtrl2D.tc.value.simPublic()
        dut.loopCtrl2D.to.value.simPublic()
        dut.loopCtrl2D.th.value.simPublic()
        dut.loopCtrl2D.tw.value.simPublic()
        dut.loopCtrl2D.ti.value.simPublic()
        dut.loopCtrl2D.kr.value.simPublic()
        dut.loopCtrl2D.ks.value.simPublic()
        dut.loopCtrl2D.sth.value.simPublic()
        dut.loopCtrl2D.stw.value.simPublic()
        dut.loopCtrl2D.od.value.simPublic()
        dut.loopCtrl2D.io.ifMapRdEn.simPublic()
        dut.loopCtrl2D.io.layerDone.simPublic()
        dut
      }
      .doSim { dut =>
        import dut._
        import dut.loopCtrl2D.{tc, to, th, tw, ti, kr, ks, sth, stw, od}
        dut.clockDomain.forkStimulus(10)
        io.config2D.assignConfig(config2D)
        io.config1D.assignConfig(config1D)
        io.config0D.assignConfig(config0D)
        io.shortCut        #= shorCut
        io.weight2DRdy     #= false
        io.weight1DRdy     #= false
        io.fMapDDRLoadDone #= false
        io.newConv         #= false
        io.addition        #= addition
        io.stem            #= stem
        io.noNext0D        #= noNext0D
        clockDomain.waitSampling()
        io.newConv         #= true
        io.weight1DRdy     #= true
        io.weight2DRdy     #= true
        io.fMapDDRLoadDone #= true
        clockDomain.waitSampling()
        io.newConv #= false

        (0 until 6000).foreach { _ =>
          clockDomain.waitSampling()
          val loop = model.Loop(
            tc.value.toBigInt,
            to.value.toBigInt,
            th.value.toBigInt,
            tw.value.toBigInt,
            ti.value.toBigInt,
            kr.value.toBigInt,
            ks.value.toBigInt,
            sth.value.toBigInt,
            stw.value.toBigInt,
            od.value.toBigInt
          )
          if (loopCtrl2D.io.ifMapRdEn.toBoolean & !loopCtrl2D.io.layerDone.toBoolean) {
            val addrAndIndex = conv2D.loopAndAddr(loop)
            assert(loopCtrl2D.io.ifMapAddr.toInt == addrAndIndex.ifAddr)
          }
        }
      }
  }

}
