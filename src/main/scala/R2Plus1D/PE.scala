package R2Plus1D

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._

import scala.language.postfixOps
import breeze.numerics.log2

case class PE(uic: Int, uoc: Int, width: Int = 8, DSPLatency: Int = 2, adderTreePipe: Boolean = true) extends Component {

  require(uoc % 2 == 0 && uic % 2 == 0)

  val io = new Bundle {
    val clr:        Bool      = in Bool ()
    val enable:     Bool      = in Bool ()
    val ifMap:      Vec[UInt] = in Vec (UInt(width bits), uic)
    val weight:     Vec[UInt] = in Vec (UInt(width bits), uic * uoc) // uoc 行，uic 列
    val partialSum: Vec[UInt] = out Vec (UInt(32 bits), uoc)
  }

  val dspArray: Array[Array[Multiplier]] =
    Array.tabulate(uoc >> 1, uic)((i: Int, j: Int) =>
      Multiplier(io.ifMap(j), io.weight(i * 2 * uic + j), io.weight((i * 2 + 1) * uic + j), DSPLatency) addAttribute ("use_dsp", "yes")
    )

  val sums: Array[UInt] =
    Array.tabulate(uoc)((u: Int) =>
      Vec(dspArray(u / 2).map((_: Multiplier).io.p(u % 2)))
        .reduceBalancedTree(_ + _, (s, l) => if (adderTreePipe && (l + 1) % 3 == 0 && (l + 1) != log2Up(uic)) RegNext(s) else s)
    )

  val psumReg: Array[UInt] = Array.fill(uoc)(Reg(UInt(32 bits)) init 0) // addAttribute ("use_dsp", "no")

  sums.zip(psumReg).foreach { case (s, p) => when(io.clr.d(PELatency - 1))(p.clearAll()) elsewhen io.enable.d(PELatency - 1)(p := p + s) }
  psumReg.zip(io.partialSum).foreach { case (p, o) => o := p }

  def PELatency: Int = {
    val levels           = log2Up(uic)
    val adderTreeLatency = if (levels % 3 == 0) levels / 3 - 1 else levels / 3
    adderTreeLatency + DSPLatency
    if (adderTreePipe) adderTreeLatency + DSPLatency else DSPLatency
  }
}
