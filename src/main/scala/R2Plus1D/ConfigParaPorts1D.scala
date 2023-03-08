package R2Plus1D

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

case class ConfigParaPorts1D() extends Bundle {
  val Nihw, Nohw:                                        UInt = UInt(8 bits)
  val Kt:                                                UInt = UInt(2 bits)
  val Nid:                                               UInt = UInt(5 bits)
  val Nod:                                               UInt = UInt(5 bits)
  val Nc, Uc, Noc, Toc:                                  UInt = UInt(11 bits)
  val NicDUicCeil, TocDUocCeil, NocDTcCeil, NocDUocCeil: UInt = UInt(8 bits)
  val stride:                                            Bool = Bool()
  val padding:                                           UInt = UInt(2 bits)
  val ifMapSize:                                         UInt = UInt(log2Up(Parameter.ifMapSizeMax1D + 1) bits)
  val ofMapSizeOwOh:                                     UInt = UInt(log2Up(Parameter.ofMapMaxOwOhSize1D + 1) bits)
  val ofMapSize:                                         UInt = UInt(log2Up(Parameter.ofMapSizeMax1D) bits)

  def assignConfig(c: model.ConvConfig): Unit = {
    Nihw          #= c.Nihw
    Nohw          #= c.Nohw
    Kt            #= c.K
    Nid           #= c.Nid
    Nod           #= c.Nod
    Nc            #= c.Nic
    Uc            #= c.Uic
    Noc           #= c.Noc
    Toc           #= c.Tc
    NicDUicCeil   #= c.NicDUicCeil
    TocDUocCeil   #= c.TcDUocCeil
    NocDTcCeil    #= c.NocDTcCeil
    stride        #= c.stride > 1
    padding       #= c.padding
    ifMapSize     #= c.ifMapSize
    ofMapSize     #= c.ofMapSize
    ofMapSizeOwOh #= c.Nohw * c.Nohw
    NocDUocCeil   #= c.NocDUocCeil
  }
}
