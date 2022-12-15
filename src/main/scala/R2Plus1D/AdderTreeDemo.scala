package R2Plus1D

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class AdderTreeDemo(num: Int) extends Component {
  val io = new Bundle {
    val a   = in Vec (UInt(8 bits), num)
    val sum = out(a.reduceBalancedTree(_ +^ _, (s, l) => if ((l + 1) % 3 == 0 && log2Up(num) != (l + 1)) RegNext(s) else s))
  }
}
