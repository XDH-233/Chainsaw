package R2Plus1D

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw._
import R2Plus1D.model.ConvConfig

case class ConfigParaPorts0D() extends Bundle {
  val Nic         = UInt(log2Up(Parameter.NicMax + 1) bits)
  val Noc         = UInt(log2Up(Parameter.NocMax + 1) bits)
  val Nihw        = UInt(log2Up(Parameter.NihwMax2D + 1) bits)
  val Nohw        = UInt(log2Up(Parameter.NohwMax1D + 1) bits)
  val Nid, Nod    = UInt(5 bits)
  val NicDUicCeil = UInt(6 bits)
  val NocDUocCeil = UInt(6 bits)
  val ifMapSize   = UInt(log2Up(Parameter.ifMapSizeMax2D + 1) bits)
  val ofMapSize   = UInt(log2Up(Parameter.ofMapSizeMax1D + 1) bits)

  def assignConfig(config: ConvConfig) = {
    Nic         #= config.Nic
    Noc         #= config.Noc
    Nihw        #= config.Nihw
    Nohw        #= config.Nohw
    Nid         #= config.Nid
    Nod         #= config.Nod
    NicDUicCeil #= config.NicDUicCeil
    NocDUocCeil #= config.NocDUocCeil
    ifMapSize   #= config.ifMapSize
    ofMapSize   #= config.ofMapSize
  }
}
