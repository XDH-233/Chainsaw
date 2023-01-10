package R2Plus1D

import spinal.core._
import spinal.lib._
import Parameter._
import scala.math.sqrt

import scala.language.postfixOps

case class LoopInsideTile2D() extends Component {
  val io = new Bundle {
    // config
    val loadConfig:  Bool = in Bool ()
    val Nic:         UInt = in UInt (11 bits)
    val Tc:          UInt = in UInt (11 bits)
    val Nd:          UInt = in UInt (5 bits)
    val Nihw:        UInt = in UInt (7 bits)
    val Nohw:        UInt = in UInt (6 bits)
    val Krs:         UInt = in UInt (2 bits)
    val Stride:      Bool = in Bool () // Ture -> 2, False -> 1
    val paddingSize: UInt = in UInt (2 bits)

//    val weightAddr:       UInt = out UInt (featureMapBufferAddrWidth bits)
//    val featureMapAddr:   UInt = out UInt (weightBuffer2DAddrWidth bits)
//    val outputBufferAddr: UInt = out UInt (outputBufferAddrWidth bits)

  }
  private val NohwTile = sqrt(Uc / 2).toInt //  sqrt(128/2) = 8

  // od
//  val odCounter =

  def divideCeil(divider: UInt, divisor: Int): UInt = {
    require(isPow2(divisor))
    val shiftRes = divider >> log2Up(divisor)
    shiftRes + (divider > divisor).asUInt
  }

}
