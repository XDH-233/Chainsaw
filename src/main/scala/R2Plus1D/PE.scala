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
    val ifMap:      Vec[SInt] = in Vec (SInt(width bits), uic)
    val weight:     Vec[SInt] = in Vec (SInt(width bits), uic * uoc) // uoc 行，uic 列
    val partialSum: Vec[SInt] = out Vec (SInt(log2Up(uic) + width * 2 bits), uoc)
  }

  val dspArray: Array[Array[Multiplier]] =
    Array.tabulate(uoc >> 1, uic)((i: Int, j: Int) =>
      Multiplier(io.ifMap(j), io.weight(i * 2 * uic + j), io.weight((i * 2 + 1) * uic + j), DSPLatency) addAttribute ("use_dsp", "yes")
    )

  val sums: Array[SInt] =
    Array.tabulate(uoc)((u: Int) =>
      Vec(dspArray(u / 2).map((_: Multiplier).io.p(u % 2)))
        .reduceBalancedTree(_ +^ _, (s, l) => if (adderTreePipe && (l + 1) % 3 == 0 && (l + 1) != log2Up(uic)) RegNext(s) else s)
    )

  io.partialSum.zip(sums).foreach { case (p, s) => p := RegNext(s) }

  def PELatency: Int = {
    val levels           = log2Up(uic)
    val adderTreeLatency = if (levels % 3 == 0) levels / 3 - 1 else levels / 3
    if (adderTreePipe) adderTreeLatency + DSPLatency + 1 else DSPLatency + 1
  }
}
