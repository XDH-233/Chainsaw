package R2Plus1D
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.regif.{AxiLite4BusInterface, HtmlGenerator, RegInst}

case class HostController() extends Component { // TODO
  val io = new Bundle {
    val lite: AxiLite4 = slave(AxiLite4(AxiLite4Config(7, 32))) // 总线的地址位宽为7 位，数据位宽位32位
  }
  val busInterface: AxiLite4BusInterface = AxiLite4BusInterface(io.lite, (1, 1), "config")

  val TIC: RegInst = busInterface.newReg("Tic")
  val TC:  RegInst = busInterface.newReg("Tc")

  val NIC: RegInst = busInterface.newReg("Nic")
  val NC:  RegInst = busInterface.newReg("Nc")
  val NOC: RegInst = busInterface.newReg("Noc")

  val NID:  RegInst = busInterface.newReg("Nid")
  val NIHW: RegInst = busInterface.newReg("Nihw")
  val NKRS: RegInst = busInterface.newReg("Nkrs")
  val ND:   RegInst = busInterface.newReg("ND")
  val NHW:  RegInst = busInterface.newReg("Nhw")
  val NOD:  RegInst = busInterface.newReg("Nod")
  val NOHW: RegInst = busInterface.newReg("Nohw")
  val NKT:  RegInst = busInterface.newReg("Nkt")

  val Nic1:  RegInst = busInterface.newReg("NIC1")
  val NOC1:  RegInst = busInterface.newReg("Noc1")
  val NID1:  RegInst = busInterface.newReg("Nid1")
  val NIHW1: RegInst = busInterface.newReg("Nihw1")
  val NOD1:  RegInst = busInterface.newReg("Nod1")
  val NOHW1: RegInst = busInterface.newReg("Nohw1")

  busInterface.accept(HtmlGenerator("config", "host_controller_regs"))
}

object GenHtml extends App {
  SpinalVerilog(HostController())
}
