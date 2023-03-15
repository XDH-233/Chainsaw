package R2Plus1D

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps
import Chainsaw._

case class GlobalAvgPooling(uoc: Int, dataInWidth: Int, ofMapSize: Int, dataOutWidth: Int) extends Component {
  val io = new Bundle {
    val din:  Flow[Fragment[Vec[AFix]]] = slave Flow (Fragment(Vec(AFix.S(dataInWidth bits), uoc)))
    val dout: Flow[Vec[AFix]]           = master Flow (Vec(AFix.S(dataOutWidth bits), uoc))
  }
  val regs: Vec[AFix] = Vec(Reg(AFix.S(dataInWidth + log2Up(ofMapSize) bits)) init (0), uoc)
  regs.zip(io.din.payload).foreach { case (r, d) =>
    when(io.din.last) {
      r := BigDecimal(0)
    } elsewhen (io.din.valid) {
      r := r +| d
    }
  }

  val divisor: AFix = AFix.U(0 exp, -16 exp)
  divisor := 1.0 / ofMapSize
  val quotients: IndexedSeq[AFix] = io.din.payload.zip(regs).map { case (i, r) => RegNextWhen(((r +| i) * divisor), io.din.last) }

  io.dout.payload.zip(quotients).foreach { case (o, q) => o := q.truncate().sat((1 << dataOutWidth / 2) - 1, -(1 << dataOutWidth / 2)) }
  val outValid: Bool = RegInit(False)
  when(io.din.last) {
    outValid.set()
  }
  io.dout.valid := outValid

}
