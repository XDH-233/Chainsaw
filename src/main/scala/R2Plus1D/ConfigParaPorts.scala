package R2Plus1D
import spinal.core._

import scala.language.postfixOps
import Chainsaw._
import R2Plus1D.Parameter.ifMapSizeMax2D
import spinal.lib._

class ConfigParaPorts(width: Int) extends Bundle with IMasterSlave {
  val Nic, Nc, Nohw, Nd, Krs, Tow, Toh, Nihw: UInt = UInt(width bits)
  val NcDUcCeil, NicDUicCeil:                 UInt = UInt(4 bits)
  val kernelSize:                             UInt = UInt(6 bits)
  val ifMapSize:                              UInt = UInt(log2Up(ifMapSizeMax2D) bits)
  val NohwDTohCei:                            UInt = UInt(width bits)
  val NohwDTowCei:                            UInt = UInt(width bits)
  val stride:                                 Bool = Bool() // 0 -> 1, 1 -> 2
  val padding:                                UInt = UInt(3 bits)
  override def asMaster(): Unit = {
    out(Nic, Nc, Nohw, Nd, Krs, Tow, Toh, Nihw, NcDUcCeil, NicDUicCeil, kernelSize, ifMapSize, NohwDTowCei, NohwDTohCei, stride, padding)
  }

  override type RefOwnerType = this.type
}
