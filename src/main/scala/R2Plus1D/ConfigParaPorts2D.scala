package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw._
import R2Plus1D.Parameter.{ifMapSizeMax2D, ofMapSizeMax2D}
import spinal.lib._

case class ConfigParaPorts2D() extends Bundle {
  val Nic, Nc:                UInt = UInt(11 bits)
  val Nihw, Nohw:             UInt = UInt(7 bits)
  val Nd, Krs, Toh, Tow:      UInt = UInt(5 bits)
  val NcDUcCeil, NicDUicCeil: UInt = UInt(5 bits)
  val kernelSize:             UInt = UInt(6 bits)
  val ifMapSize:              UInt = UInt(log2Up(ifMapSizeMax2D) bits)
  val ofMapSize:              UInt = UInt(log2Up(Parameter.ofMapSizeMax2D + 1) bits)
  val NohwDTohCei:            UInt = UInt(5 bits)
  val NohwDTowCei:            UInt = UInt(5 bits)
  val stride:                 Bool = Bool() // 0 -> 1, 1 -> 2
  val padding:                UInt = UInt(3 bits)

  def assignConfig(config: model.ConvConfig): Unit = {
    Nic         #= config.Nic
    Nc          #= config.Noc
    Nohw        #= config.Nohw
    Krs         #= config.K
    Tow         #= config.Tow
    Toh         #= config.Toh
    Nihw        #= config.Nihw
    kernelSize  #= config.kernelSize
    ifMapSize   #= config.kernelSize
    NohwDTowCei #= config.NohwDTowCeil
    NohwDTohCei #= config.NohwDTohCeil
    NcDUcCeil   #= config.NocDUocCeil
    NicDUicCeil #= config.NicDUicCeil
    stride      #= config.stride > 1
    padding     #= config.padding
    ofMapSize   #= config.ofMapSize
    Nd          #= config.Nid
  }
}
