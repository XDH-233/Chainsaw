package R2Plus1D

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

class ConfigParaPorts1D() extends Bundle with IMasterSlave {
  val Nhw                    = UInt(8 bits)
  val Kt                     = UInt(2 bits)
  val Nid                    = UInt(5 bits)
  val Nod                    = UInt(5 bits)
  val Nc, Noc                = UInt(11 bits)
  val NcDUcCeil, NocDUocCeil = UInt(8 bits)
  val stride                 = Bool()
  val padding                = UInt(2 bits)
  val ifMapSize              = UInt(log2Up(Parameter.ifMapSizeMax1D + 1) bits)
  val ofMapsize2D            = UInt(log2Up(3136 + 1) bits)
  override def asMaster(): Unit = {
    out(Nhw, Kt, Nid, Nod, Nc, Noc, NcDUcCeil, NocDUocCeil, stride, padding, ifMapSize, ofMapsize2D)
  }

  def loadConfigInSim(c: model.Conv1DConfig): Unit = {
    Nhw         #= c.Nhw
    Kt          #= c.Kt
    Nid         #= c.Nid
    Nod         #= c.Nod
    Nc          #= c.Nc
    Noc         #= c.Noc
    NcDUcCeil   #= c.NcDUcCeil
    NocDUocCeil #= c.NocDUocCeil
    stride      #= c.stride > 1
    padding     #= c.padding
    ifMapSize   #= c.ifMapSize
    ofMapsize2D #= c.Nhw * c.Nhw
  }
  override type RefOwnerType = this.type
}
