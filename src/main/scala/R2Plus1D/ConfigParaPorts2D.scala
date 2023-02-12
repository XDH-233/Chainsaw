package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw._
import R2Plus1D.Parameter.{ifMapSizeMax2D, ofMapSizeMax2D}
import spinal.lib._

class ConfigParaPorts2D(width: Int) extends Bundle with IMasterSlave {
  val Nic, Nc, Nohw, Nd, Krs, Tow, Toh, Nihw: UInt = UInt(width bits)
  val NcDUcCeil, NicDUicCeil:                 UInt = UInt(4 bits)
  val kernelSize:                             UInt = UInt(6 bits)
  val ifMapSize:                              UInt = UInt(log2Up(ifMapSizeMax2D) bits)
  val ofMapSize:                              UInt = UInt(log2Up(Parameter.ofMapSizeMax2D + 1) bits)
  val NohwDTohCei:                            UInt = UInt(width bits)
  val NohwDTowCei:                            UInt = UInt(width bits)
  val stride:                                 Bool = Bool() // 0 -> 1, 1 -> 2
  val padding:                                UInt = UInt(3 bits)
  override def asMaster(): Unit = {
    out(Nic, Nc, Nohw, Nd, Krs, Tow, Toh, Nihw, NcDUcCeil, NicDUicCeil, kernelSize, ifMapSize, NohwDTowCei, NohwDTohCei, stride, padding, ofMapSize)
  }

  def assignConfig(config: model.Conv2DConfig): Unit = {
    Nic         #= config.Nic
    Nc          #= config.Nc
    Nohw        #= config.Nohw
    Krs         #= config.Krs
    Tow         #= config.Tow
    Toh         #= config.Toh
    Nihw        #= config.Nihw
    kernelSize  #= config.kernelSize
    ifMapSize   #= config.kernelSize
    NohwDTowCei #= config.NohwDTowCeil
    NohwDTohCei #= config.NohwDTohCeil
    NcDUcCeil   #= config.NcDUcCeil
    NicDUicCeil #= config.NicDUicCeil
    stride      #= config.stride > 1
    padding     #= config.padding
    ofMapSize   #= config.ofMapSize
    Nd          #= config.Nd
  }
  override type RefOwnerType = this.type
}
