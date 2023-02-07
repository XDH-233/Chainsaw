package R2Plus1D

import spinal.core._
import Parameter._

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

  }

  val PE2D:             PE               = PE(uic = uic, uoc = uc, width = dataWidth)
  val weightBuffer2D:   WeightBuffer     = WeightBuffer(dataWidth = dataWidth, depth = weightBuffer2DDepth, uic = uic)
  val featureMapBuffer: FeatureMapBuffer = FeatureMapBuffer(width = dataWidth, depth = featureMapDepth, uic = uic)
  val pingPongRegs2D:   PingPongRegs2D   = PingPongRegs2D(width = dataWidth, uoc = uc, uic = uic)
  val loopCtrl2D:       LoopCtrl2D       = LoopCtrl2D(uic = uic, uc = uc)

  // weight write
  weightBuffer2D.io.writeEn   := io.weightWriteEn
  weightBuffer2D.io.writeAddr := io.weightWriteAddr
  weightBuffer2D.io.writeData := io.weightWriteData
  weightBuffer2D.io.switch    := io.weightSwitch

  // ping-ping regs read from weightBuffer2D
  weightBuffer2D.io.readEn   := pingPongRegs2D.io.weightBufferReadEn
  weightBuffer2D.io.readAddr := pingPongRegs2D.io.readAddr
  pingPongRegs2D.io.weightIn := weightBuffer2D.io.readData

  // feature map buffer write
  featureMapBuffer.io.switch := io.fMapSwitch
  featureMapBuffer.io.we     := io.fMapWe
  featureMapBuffer.io.wAddr  := io.fMapWAddr
  featureMapBuffer.io.wData  := io.fMapWData

  // feature map read -> PE
  PE2D.io.ifMap.zip(featureMapBuffer.io.rData2DPE.subdivideIn(dataWidth bits)).foreach { case (p, f) => p := f.asSInt }

}
