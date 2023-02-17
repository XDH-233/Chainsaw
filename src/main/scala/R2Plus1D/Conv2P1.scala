package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps
import Chainsaw._
import R2Plus1D.Parameter.{featureMapDepth, outputBuffer2DDepth, weightBuffer2DDepth}

case class Conv2P1(uic: Int = Parameter.Uic, uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc, dataWidth: Int = 8) extends Component {
  val io = new Bundle {
    val weight2DRdy:     Bool = in Bool ()
    val weight1DRdy:     Bool = in Bool ()
    val fMapDDRLoadDone: Bool = in Bool ()
    val newConv:         Bool = in Bool ()

    val config2D: ConfigParaPorts2D = in(ConfigParaPorts2D())
    val config1D: ConfigParaPorts1D = in(ConfigParaPorts1D())
    val config0D: ConfigParaPorts1D = in(ConfigParaPorts1D())

    val addition:              Bool = in Bool ()
    val shortCut:              Bool = in Bool ()
    val load1DResInto0DBuffer: Bool = in Bool ()

    val weight2DSwitch:    Bool = in Bool ()
    val weight2DWriteEn:   Bool = in Bool ()
    val weight2DWriteAddr: UInt = in UInt (log2Up(Parameter.weightBuffer2DDepth) bits)
    val weight2DWriteData: Bits = in Bits (dataWidth * uic bits)

    val weight1DSwitch:    Bool = in Bool ()
    val weight1DWriteEn:   Bool = in Bool ()
    val weight1DWriteAddr: UInt = in UInt (log2Up(Parameter.weightBuffer1DDepth) bits)
    val weight1DWriteData: Bits = in Bits (dataWidth * uc bits)
  }

  val relu:             Bool             = ~io.addition
  val shortCutReg:      Bool             = RegInit(False)
  val PE2D:             PE               = PE(uic = uic, uoc = uc, width = dataWidth)
  val weightBuffer2D:   WeightBuffer     = WeightBuffer(dataWidth = dataWidth, depth = weightBuffer2DDepth, uic = uic)
  val featureMapBuffer: FeatureMapBuffer = FeatureMapBuffer(width = dataWidth, depth = featureMapDepth, uic = uic)
  val pingPongRegs2D:   PingPongRegs2D   = PingPongRegs2D(width = dataWidth, uoc = uc, uic = uic)
  val accRAM2D:         AccRAM           = AccRAM(uoc = uc, depth = uc)
  val loopCtrl2D:       LoopCtrl2D       = LoopCtrl2D(uic = uic, uc = uc, PELatency = PE2D.PELatency, readLatencyBRAM = accRAM2D.readLatency)
  val outputBuffer2D:   OutputBuffer     = OutputBuffer(dataWidth = dataWidth, uc = uc, depth = outputBuffer2DDepth)

  val weightBuffer1D: WeightBuffer = WeightBuffer(dataWidth = dataWidth, uic = uc, depth = Parameter.weightBuffer1DDepth)
  val PE1D:           PE           = PE(uic = uc, uoc = uoc, width = dataWidth)
  val accRAM1D:       AccRAM       = AccRAM(uoc = uoc, depth = Parameter.ofMapMaxOwOhSize1D, dataOutWidth = dataWidth, dataWidth = 28)
  val loopCtrl1D: LoopCtrl1D =
    LoopCtrl1D(uc = uc, uoc = uoc, readLatencyURAM = weightBuffer1D.readLatency, readLatencyBRAM = accRAM1D.readLatency, PELatency = PE1D.PELatency)
  val pingPongRegs1D:      PingPongRegs1D      = PingPongRegs1D(dataWidth = dataWidth, uc = uc, uoc = uoc)
  val outputBuffer0D:      OutputBuffer        = OutputBuffer(dataWidth = dataWidth, uc = uoc, readLatency = 4, depth = featureMapDepth)
  val elementWiseAddition: ElementWiseAddition = ElementWiseAddition(dataWidth = dataWidth, uoc = uoc, depth = Parameter.featureMapDepth)

  when(shortCutReg & ~loopCtrl2D.io.ofMapBuffer2DRdy) {
    loopCtrl1D.io.config.assignAllByName(io.config0D)
  } otherwise {
    loopCtrl1D.io.config.assignAllByName(io.config1D)
  }

  val loadConfig2D: Bool = Bool() setAsReg () init False
  val loadConfig1D: Bool = Bool() setAsReg () init False
  loopCtrl1D.io.toWriteDOne           := loopCtrl2D.io.toWriteDone
  loopCtrl1D.io.ofMapBuffer2DRdy      := loopCtrl2D.io.ofMapBuffer2DRdy
  pingPongRegs1D.io.toWriteDone       := loopCtrl2D.io.toWriteDone
  pingPongRegs1D.io.shortCut          := shortCutReg
  pingPongRegs1D.io.willOverFlowIfInc := loopCtrl1D.io.willOverflowIfInc
  val FSM = new StateMachine {
    val idle     = new State with EntryPoint
    val conv2D1D = new State
    val conv2D0D = new State

    idle.whenIsActive {
      when(io.newConv & io.weight2DRdy & io.fMapDDRLoadDone) {
        loadConfig2D.set()
        loadConfig1D.set()
        when(io.shortCut & io.weight1DRdy) {
          shortCutReg.set()
          goto(conv2D0D)
        } otherwise {
          goto(conv2D1D)
        }
      }
    }

    conv2D0D.whenIsActive {
      loadConfig2D.clear()
      loadConfig1D.clear()
      when(loopCtrl1D.io.conv1DDone) {
        loadConfig1D.set()
        shortCutReg.clear()
        goto(conv2D1D)
      }
    }

    conv2D1D.whenIsActive {
      loadConfig2D.clear()
      loadConfig1D.clear()
      when(loopCtrl1D.io.conv1DDone) {
        goto(idle)
      }
    }

  }

  // i0
  pingPongRegs2D.io.weightBufferRdy := io.weight2DRdy
  pingPongRegs2D.io.ifMapRdy        := io.fMapDDRLoadDone
  pingPongRegs1D.io.weightRdy       := io.weight1DRdy
  loopCtrl1D.io.loadConfig          := loadConfig1D
  loopCtrl2D.io.loadConfig          := loadConfig2D
  loopCtrl1D.io.shortCut            := shortCutReg
  loopCtrl2D.io.config.assignAllByName(io.config2D)

  // weight write
  weightBuffer2D.io.writeEn   := io.weight2DWriteEn
  weightBuffer2D.io.writeAddr := io.weight2DWriteAddr
  weightBuffer2D.io.writeData := io.weight2DWriteData
  weightBuffer2D.io.switch    := io.weight2DSwitch

  // ping-ping regs 2D read from weightBuffer2D
  weightBuffer2D.io.readEn   := pingPongRegs2D.io.readEn
  weightBuffer2D.io.readAddr := pingPongRegs2D.io.readAddr
  pingPongRegs2D.io.weightIn := weightBuffer2D.io.readData

  // ping-pong2D regs and loopCtrl2D
  loopCtrl2D.io.readDone            := pingPongRegs2D.io.readDone
  pingPongRegs2D.io.weightAddrBase  := loopCtrl2D.io.weightAddrBase
  pingPongRegs2D.io.weightLoadedNum := loopCtrl2D.io.weightLoadedNum
  loopCtrl2D.io.filled              := pingPongRegs2D.io.filled
  pingPongRegs2D.io.layerDone       := loopCtrl2D.io.layerDone

  // feature map buffer write
  featureMapBuffer.io.switch.clear() // FIXME
  featureMapBuffer.io.we    := loopCtrl1D.io.ofMapWriteEn
  featureMapBuffer.io.wAddr := loopCtrl1D.io.ofMapAddr
  featureMapBuffer.io.wData := accRAM1D.io.doutReLU

  // feature map read 2D
  featureMapBuffer.io.readEn2DPE := loopCtrl2D.io.ifMapRdEn // ping pong ready
  featureMapBuffer.io.rAddr2DPE  := loopCtrl2D.io.ifMapAddr.asUInt.resized // addr
  PE2D.io.ifMap.zip(featureMapBuffer.io.rData2DPE).foreach { case (p, f) => p := f.asSInt } // read data
  featureMapBuffer.io.rAddr2DPEVld := loopCtrl2D.io.ifMapAddrVld

  // feature map read 1D
  featureMapBuffer.io.readEn1DPE   := loopCtrl1D.io.ifMapRdEn
  featureMapBuffer.io.rAddr1DPE    := loopCtrl1D.io.ifMapAddr.resized
  featureMapBuffer.io.rAddr1DPEVld := loopCtrl1D.io.ifMapAddrVld

  // accRAM2D
  accRAM2D.io.writeAcc := loopCtrl2D.io.writeAcc
  accRAM2D.io.addr     := loopCtrl2D.io.accRamAddr
  accRAM2D.io.doutEn   := loopCtrl2D.io.doutEn
  accRAM2D.io.wData.zip(PE2D.io.partialSum).foreach { case (w, p) => w := p.resized }

  // PE2D <- ping-pong out
  val weightOut2D: Seq[Vec[Bits]] = pingPongRegs2D.io.weightOut.map(_.subdivideIn(dataWidth bits))
  PE2D.io.weight.zipWithIndex.foreach { case (p, i) => p := weightOut2D(i / uic)(i % uic).asSInt }

  // output buffer 2D read and write
  outputBuffer2D.io.we    := loopCtrl2D.io.ofMapWe & loopCtrl2D.io.ofMapAddrVld
  outputBuffer2D.io.wAddr := loopCtrl2D.io.ofMapAddr
  outputBuffer2D.io.wData := accRAM2D.io.doutReLU

  outputBuffer2D.io.rdEn     := loopCtrl1D.io.ifMapRdEn
  outputBuffer2D.io.rAddr    := loopCtrl1D.io.ifMapAddr
  outputBuffer2D.io.rAddrVld := loopCtrl1D.io.ifMapAddrVld

  // weight buffer 1D
  // weight buffer write
  weightBuffer1D.io.switch    := io.weight1DSwitch
  weightBuffer1D.io.writeEn   := io.weight1DWriteEn
  weightBuffer1D.io.writeAddr := io.weight1DWriteAddr
  weightBuffer1D.io.writeData := io.weight1DWriteData

  // weight buffer read -> ping-ping regs
  pingPongRegs1D.io.weightIn := weightBuffer1D.io.readData
  weightBuffer1D.io.readEn   := pingPongRegs1D.io.readEn
  weightBuffer1D.io.readAddr := pingPongRegs1D.io.weightAddr

  // accRAM1D
  // accRam1D
  accRAM1D.io.writeAcc := loopCtrl1D.io.writeAcc
  accRAM1D.io.addr     := loopCtrl1D.io.accAddr
  accRAM1D.io.doutEn   := loopCtrl1D.io.doutEn
  accRAM1D.io.wData.zip(PE1D.io.partialSum).foreach { case (w, p) => w := p.resized }

  // PE 1D <- output buffer 2D or feature map
  when(shortCutReg) {
    (uic until uc).foreach(u => PE1D.io.ifMap(u).clearAll())
    featureMapBuffer.io.rData1DPE.zipWithIndex.foreach { case (f, i) => PE1D.io.ifMap(i) := f.asSInt }
  } otherwise {
    PE1D.io.ifMap.zip(outputBuffer2D.io.rData).foreach { case (p, i) => p := i.asSInt }
  }
  val weightOut1D: Seq[Vec[Bits]] = pingPongRegs1D.io.weightOut.map(_.subdivideIn(dataWidth bits))
  PE1D.io.weight.zipWithIndex.foreach { case (p, i) => p := weightOut1D(i / uc)(i % uc).asSInt }

  // ping pong regs 1D and loop ctrl 1D
  loopCtrl1D.io.weightFilled       := pingPongRegs1D.io.weightFilled
  pingPongRegs1D.io.loadConfig     := loadConfig1D
  pingPongRegs1D.io.layerDone      := loopCtrl1D.io.layerReadDone
  pingPongRegs1D.io.ofMapSize2D    := io.config1D.ofMapSizeOwOh
  pingPongRegs1D.io.weightLoadNum  := loopCtrl1D.io.weightLoadedNum
  pingPongRegs1D.io.weightAddrBase := loopCtrl1D.io.weightAddrBase
  pingPongRegs1D.io.ofMap2DRdy     := loopCtrl2D.io.ofMapBuffer2DRdy
  pingPongRegs1D.io.fMapDDRRdy     := io.fMapDDRLoadDone
  loopCtrl1D.io.readDone           := pingPongRegs1D.io.readDone

  // outputBuffer0D
  outputBuffer0D.io.we    := loopCtrl1D.io.ofMapWriteEn & io.load1DResInto0DBuffer
  outputBuffer0D.io.wAddr := loopCtrl1D.io.ofMapAddr
  outputBuffer0D.io.wData := accRAM1D.io.doutReLU

  outputBuffer0D.io.rdEn  := elementWiseAddition.io.buffer0DRdEn
  outputBuffer0D.io.rAddr := elementWiseAddition.io.buffer0DRAddr
  outputBuffer0D.io.rAddrVld.clear()

  // element-wise addition
  elementWiseAddition.io.ofMapWe1D := loopCtrl1D.io.ofMapWriteEn
  elementWiseAddition.io.ofMapAddr := loopCtrl1D.io.ofMapAddr
  Function.vecZip(elementWiseAddition.io.accRAMDout, accRAM1D.io.douts)
  elementWiseAddition.io.buffer0DRData := outputBuffer0D.io.rData
}
