package R2Plus1D

import spinal.core._
import Parameter._
import spinal.lib._

import scala.language.postfixOps

case class Conv2DTop(dataWidth: Int = 8, uic: Int = Uic, uc: Int = Uc) extends Component {
  // 这个模块不写逻辑，只连线，逻辑写在各自的模块中
  val io = new Bundle {
    val weightSwitch:    Bool = in Bool ()
    val weightWriteEn:   Bool = in Bool ()
    val weightWriteAddr: UInt = in UInt (log2Up(weightBuffer2DDepth) bits)
    val weightWriteData: Bits = in Bits (dataWidth * uic bits)

    val fMapSwitch: Bool = in Bool ()
    val fMapWe:     Bool = in Bool ()
    val fMapWData:  Bits = in Bits (dataWidth * uic bits)
    val fMapWAddr:  UInt = in UInt (log2Up(featureMapDepth) bits)

    val weightBufferRdy: Bool            = in Bool ()
    val loadConfig:      Bool            = in Bool ()
    val configParaPorts: ConfigParaPorts = slave(new ConfigParaPorts(16))
  }

  val PE2D:             PE               = PE(uic = uic, uoc = uc, width = dataWidth)
  val weightBuffer2D:   WeightBuffer     = WeightBuffer(dataWidth = dataWidth, depth = weightBuffer2DDepth, uic = uic)
  val featureMapBuffer: FeatureMapBuffer = FeatureMapBuffer(width = dataWidth, depth = featureMapDepth, uic = uic)
  val pingPongRegs2D:   PingPongRegs2D   = PingPongRegs2D(width = dataWidth, uoc = uc, uic = uic)
  val loopCtrl2D:       LoopCtrl2D       = LoopCtrl2D(uic = uic, uc = uc)

  // i0
  pingPongRegs2D.io.weightBufferRdy := io.weightBufferRdy
  loopCtrl2D.io.configParaPorts.assignAllByName(io.configParaPorts)
  loopCtrl2D.io.loadConfig   := io.loadConfig
  featureMapBuffer.io.switch := io.fMapSwitch

  // weight write
  weightBuffer2D.io.writeEn   := io.weightWriteEn
  weightBuffer2D.io.writeAddr := io.weightWriteAddr
  weightBuffer2D.io.writeData := io.weightWriteData
  weightBuffer2D.io.switch    := io.weightSwitch

  // ping-ping regs read from weightBuffer2D
  weightBuffer2D.io.readEn   := pingPongRegs2D.io.readEn
  weightBuffer2D.io.readAddr := pingPongRegs2D.io.readAddr
  pingPongRegs2D.io.weightIn := weightBuffer2D.io.readData

  // ping-pong regs and loopCtrl
  loopCtrl2D.io.readDone            := pingPongRegs2D.io.readDone
  pingPongRegs2D.io.weightAddrBase  := loopCtrl2D.io.weightAddrBase
  pingPongRegs2D.io.weightLoadedNum := loopCtrl2D.io.weightLoadedNum
  loopCtrl2D.io.filled              := pingPongRegs2D.io.filled

  // feature map buffer write
  featureMapBuffer.io.we    := io.fMapWe
  featureMapBuffer.io.wAddr := io.fMapWAddr
  featureMapBuffer.io.wData := io.fMapWData

  // feature map read 2D
  featureMapBuffer.io.readEn2DPE := pingPongRegs2D.io.filled // ping pong ready
  featureMapBuffer.io.rAddr2DPE  := loopCtrl2D.io.ifMapAddr.asUInt.resized // addr
  PE2D.io.ifMap.zip(featureMapBuffer.io.rData2DPE).foreach { case (p, f) => p := f.asSInt } // read data
  featureMapBuffer.io.rAddr2DPEVld := loopCtrl2D.io.ifMapAddrVld

  // feature map read 1D
  featureMapBuffer.io.readEn1DPE.clear()
  featureMapBuffer.io.rAddr1DPE.clearAll()

}
