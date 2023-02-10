package R2Plus1D
import spinal.core._
import scala.language.postfixOps
import Chainsaw._

object Function {
  def shiftLoad(p: Vec[Bits], w: Bits, en: Bool): Unit = {
    when(en) {
      p.head := w
      p.tail.zip(p.init).foreach { case (t, i) => t := i }
    }
  }

  def vecZip(a: Vec[Bits], b: Vec[Bits]): Unit = {
    a.zip(b).foreach { case (p, q) => p := q }
  }

  def pingPongLoadAndOut(ping: Vec[Bits], pong: Vec[Bits], wIn: Bits, wOut: Vec[Bits], state: Bool, en: Bool): Unit = {
    when(state) {
      shiftLoad(ping, wIn, en)
      vecZip(wOut, pong)
    } otherwise {
      shiftLoad(pong, wIn, en)
      vecZip(wOut, ping)
    }
  }
}
