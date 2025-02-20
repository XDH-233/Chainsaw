package R2Plus1D
import spinal.core._
import scala.language.postfixOps
import Chainsaw._
import spinal.lib._

object Function {
  def shiftLoad(p: Vec[Bits], w: Bits, en: Bool): Unit = {
    when(en) {
      p.head := w
      p.tail.zip(p.init).foreach { case (t, i) => t := i }
    }
  }

  def pingPongLoadAndOut(ping: Vec[Bits], pong: Vec[Bits], wIn: Bits, wOut: Vec[Bits], state: Bool, en: Bool): Unit = {
    when(state) {
      shiftLoad(ping, wIn, en)
      wOut := pong
    } otherwise {
      shiftLoad(pong, wIn, en)
      wOut := ping
    }
  }

  def CounterSum(g: GeneralCounter*): UInt = {
    Vec(g.map(_.value)).reduceBalancedTree(_ + _)
  }

  implicit class SIntExtension(s: SInt) {
    val dataWidth: Int = s.getWidth
    def saturationResize(width: Int): SInt = {
      val maxValue: Int = (1 << (width - 1)) - 1
      val minValue: Int = -(1 << (width - 1))
      if (dataWidth <= width) {
        s.resize(dataWidth)
      } else {
        val t = SInt(width bits)
        when(s < minValue) {
          t := S(minValue)
        } elsewhen (s > maxValue) {
          t := S(maxValue)
        } otherwise {
          t := s.resize(width)
        }
        t
      }
    }

    def relu(width: Int): SInt = {
      if (dataWidth <= width) {
        Mux(s.sign, S(0), s.resize(dataWidth))
      } else {
        val maxValue: Int = (1 << (width - 1)) - 1
        val t = SInt(width bits)
        when(s.sign) {
          t := S(0)
        } elsewhen (s > maxValue) {
          t := S(maxValue)
        } otherwise {
          t := s.resize(width)
        }
        t
      }
    }
  }
}
