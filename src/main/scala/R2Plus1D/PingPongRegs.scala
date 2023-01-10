package R2Plus1D

import spinal.core._

import scala.language.postfixOps

case class PingPongRegs(uic: Int = 64, uoc: Int = 128, width: Int = 8) extends Component {
  val io = new Bundle {
    val weightIn:  Bits      = in Bits (uic * width bits)
    val switch:    Bool      = in Bool ()
    val load:      Bool      = in Bool ()
    val weightOut: Vec[Bits] = out Vec (Bits(uic * width bits), uoc)
  }

  val state: Bool = RegInit(False) // 0 -> read regs0, 1 -> read regs1
  state.toggleWhen(io.switch)

  val regs: Seq[Vec[Bits]] = Seq.fill(2)(Vec(Reg(Bits(uic * width bits)), uoc))

  io.weightOut.zip(regs.head).zip(regs.last).foreach { case ((w: Bits, ping: Bits), pong: Bits) => w := Mux(state, pong, ping) }

  when(io.switch) {
    when(state) { // read regs1 now and clear regs1 then
      regs.last.foreach(_.clearAll())
    } otherwise {
      regs.head.foreach(_.clearAll())
    }
  }

  when(state) { // load regs0
    when(io.load) {
      regs.head.head := io.weightIn
      regs.head.init.zip(regs.head.tail).foreach { case (l, r) => r := l }
    }
  } otherwise { // load regs1
    when(io.load) {
      regs.last.head := io.weightIn
      regs.last.init.zip(regs.last.tail).foreach { case (l, r) => r := l }
    }
  }
}
