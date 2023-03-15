package R2Plus1D
import spinal.core._
import spinal.lib._
import scala.language.postfixOps
import Chainsaw._
import Chainsaw.memory._

case class Conv1DTop(uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc, dataWidth: Int = 8) extends Component {
  val io = new Bundle {
    val weightSwitch:    Bool = in Bool ()
    val weightWriteEn:   Bool = in Bool ()
    val weightWriteAddr: UInt = in UInt (log2Up(Parameter.weightBuffer1DDepth) bits)
    val weightWriteData: Bits = in Bits (dataWidth * uc bits)
    val weightRdy:       Bool = in Bool ()

    val we:       Bool = in Bool ()
    val wAddr:    UInt = in UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val wData:    Bits = in Bits (dataWidth * uc bits)
    val ifMapRdy: Bool = in Bool ()

    val configPorts: ConfigParaPorts1D = in(ConfigParaPorts1D())
    val loadConfig:  Bool              = in Bool ()
    val shortCut:    Bool              = in Bool ()

  }

  val outputBuffer2D: OutputBuffer = OutputBuffer(dataWidth = dataWidth, uc = uc)
  val weightBuffer1D: WeightBuffer = WeightBuffer(dataWidth = dataWidth, uic = uc, depth = Parameter.weightBuffer1DDepth)
  val PE1D:           PE           = PE(uic = uc, uoc = uoc, width = dataWidth)
  val accRAM1D:       AccRAM       = AccRAM(uoc = uoc, depth = Parameter.ofMapMaxOwOhSize1D, dataOutWidth = dataWidth)
  val loopCtrl1D: LoopCtrl1D =
    LoopCtrl1D(uoc = uoc, readLatencyURAM = weightBuffer1D.pipeRegsCount, readLatencyBRAM = accRAM1D.pipeRegCount, PELatency = PE1D.PELatency)
  val pingPongRegs1D:   PingPongRegs1D = PingPongRegs1D(dataWidth = dataWidth, uc = uc, uoc = uoc)
  val featureMapBuffer: InputBuffer2D  = InputBuffer2D(width = dataWidth, depth = Parameter.featureMapDepth, uic = uoc)

  // io
  loopCtrl1D.io.config.assignAllByName(io.configPorts)
  loopCtrl1D.io.loadConfig      := io.loadConfig
  pingPongRegs1D.io.loadConfig  := io.loadConfig
  pingPongRegs1D.io.ofMapSize2D := io.configPorts.ofMapSizeOwOh
  pingPongRegs1D.io.weightRdy   := io.weightRdy
  pingPongRegs1D.io.fMapDDRRdy  := io.ifMapRdy
  loopCtrl1D.io.shortCut        := io.shortCut

  // weight buffer write
  weightBuffer1D.io.switch    := io.weightSwitch
  weightBuffer1D.io.writeEn   := io.weightWriteEn
  weightBuffer1D.io.writeAddr := io.weightWriteAddr
  weightBuffer1D.io.writeData := io.weightWriteData

  // weight buffer read -> ping-ping regs
  pingPongRegs1D.io.weightIn := weightBuffer1D.io.readData
  weightBuffer1D.io.readEn   := pingPongRegs1D.io.readEn
  weightBuffer1D.io.readAddr := pingPongRegs1D.io.weightAddr

  // loopCtrl <-> ping-ping regs
  loopCtrl1D.io.readDone           := pingPongRegs1D.io.readDone
  pingPongRegs1D.io.weightAddrBase := loopCtrl1D.io.weightAddrBase
  pingPongRegs1D.io.weightLoadNum  := loopCtrl1D.io.weightLoadedNum
  loopCtrl1D.io.weightFilled       := pingPongRegs1D.io.weightFilled
  pingPongRegs1D.io.layerDone      := loopCtrl1D.io.conv1dDone

  // output buffer 2D write
  outputBuffer2D.io.we    := io.we
  outputBuffer2D.io.wData := io.wData
  outputBuffer2D.io.wAddr := io.wAddr
  // output buffer 2D read <- PE, loopCtrl
  outputBuffer2D.io.rdEn     := loopCtrl1D.io.ifMapRdEn
  outputBuffer2D.io.rAddr    := loopCtrl1D.io.ifMapAddr
  outputBuffer2D.io.rAddrVld := loopCtrl1D.io.ifMapAddrVld

  // accRam1D
  accRAM1D.io.writeAcc := loopCtrl1D.io.writeAcc
  accRAM1D.io.addr     := loopCtrl1D.io.accAddr
  accRAM1D.io.doutEn   := loopCtrl1D.io.doutEn
  accRAM1D.io.wData.zip(PE1D.io.partialSum).foreach { case (w, p) => w := p.resized }

  // PE1D <- outputBuffer2D, ping-pong regs
  PE1D.io.ifMap.zip(outputBuffer2D.io.rData).foreach { case (p, i) => p := i.asSInt }
  val weightOut: Seq[Vec[Bits]] = pingPongRegs1D.io.weightOut.toSeq.map(_.subdivideIn(dataWidth bits))
  PE1D.io.weight.zipWithIndex.foreach { case (p, i) => p := weightOut(i / uc)(i % uc).asSInt }
  // feature map write
  featureMapBuffer.io.wData := accRAM1D.io.doutReLU
  featureMapBuffer.io.wAddr := loopCtrl1D.io.ofMapAddr

  // feature map read
  featureMapBuffer.io.readEn2DPE.clear()
  featureMapBuffer.io.readEn1DPE.clear()
  featureMapBuffer.io.rAddr2DPE.clearAll()
  featureMapBuffer.io.rAddr1DPE.clearAll()
  featureMapBuffer.io.rAddr2DPEVld.clear()
}
