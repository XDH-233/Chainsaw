package R2Plus1D
import spinal.core._

import scala.language.postfixOps
import Chainsaw._
import Chainsaw.memory._

case class Conv1DTop(uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc, dataWidth: Int = 8) extends Component {
  val io = new Bundle {
    val weightSwitch:    Bool = in Bool ()
    val weightWriteEn:   Bool = in Bool ()
    val weightWriteAddr: UInt = in UInt (log2Up(Parameter.weightBuffer1Depth) bits)
    val weightWriteData: Bits = in Bits (dataWidth * uc bits)

  }

  val outputBuffer2D: OutputBuffer2D = OutputBuffer2D(dataWidth = dataWidth * uc, depth = Parameter.outputBuffer2DDepth)
  val weightBuffer1D: WeightBuffer   = WeightBuffer(dataWidth = dataWidth, uic = uc, depth = Parameter.weightBuffer1Depth)
  val PE1D:           PE             = PE(uic = uc, uoc = uoc, width = dataWidth)
  val loopCtrl1D:     LoopCtrl1D     = LoopCtrl1D(uc = uc, uoc = uoc)
  val pingPongRegs1D: PingPongRegs1D = PingPongRegs1D(dataWidth = dataWidth, uc = uc, uoc = uoc)

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

  // output buffer 2D write

  // output buffer 2D read <- PE, loopCtrl

  // TODO
}
