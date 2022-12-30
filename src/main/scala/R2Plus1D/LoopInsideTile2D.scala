package R2Plus1D

import spinal.core._
import Parameter._

import scala.language.postfixOps

case class LoopInsideTile2D() extends Component {
  val io = new Bundle {
    // config
    val loadConfig:  Bool = in Bool ()
    val Tic:         UInt = in UInt (11 bits)
    val Toc:         UInt = in UInt (11 bits)
    val Nid:         UInt = in UInt (5 bits)
    val Nihw:        UInt = in UInt (7 bits)
    val Nod:         UInt = in UInt (5 bits)
    val Nohw:        UInt = in UInt (6 bits)
    val Krs:         UInt = in UInt (2 bits)
    val Stride:      Bool = in Bool () // Ture -> 2, False -> 1
    val paddingSize: UInt = in UInt (2 bits)
    //

//    val weightAddr:       UInt = out UInt (featureMapBufferAddrWidth bits)
//    val featureMapAddr:   UInt = out UInt (weightBuffer2DAddrWidth bits)
//    val outputBufferAddr: UInt = out UInt (outputBufferAddrWidth bits)

  }
  private val subOutputMapSize = math.sqrt(Uc / 2).toInt //  sqrt(128/2) = 8

  def divideCeil(divider: UInt, divisor: Int): UInt = {
    require(isPow2(divisor))
    val shiftRes = divider >> log2Up(divisor)
    shiftRes + (divider > divisor).asUInt
  }

}
