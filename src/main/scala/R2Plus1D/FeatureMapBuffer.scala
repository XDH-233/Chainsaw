package R2Plus1D
import spinal.core._
import Chainsaw.memory._
import Chainsaw._
import spinal.lib._

import scala.language.postfixOps

case class FeatureMapBuffer(width: Int = 512, depth: Int = 50176, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val switch: Bool = in Bool ()
    val we:     Bool = in Bool ()
    val wData:  Bits = in Bits (width bits)
    val wAddr:  UInt = in UInt (log2Up(depth) bits)

    val readEn2DPE: Bool = in Bool ()
    val readEn1DPE: Bool = in Bool ()
    val rAddr2DPE:  UInt = in UInt (log2Up(depth) bits)
    val rAddr1DPE:  UInt = in UInt (log2Up(depth) bits)

    val rData2DPE: Bits = out Bits (width bits)
    val rData1DPE: Bits = out Bits (width bits)
  }

  val urams = Seq.fill(2)(TDPURAM(width = width, depth = depth))

  val state = RegInit(False) // 0 -> read urams0, write urams1
  state.toggleWhen(io.switch)

  when(state) { // read urams1, write urams0
    urams.head.portWrite(io.we, io.we, io.wAddr, io.wData, "a")
    urams.head.portClose("b")

    urams.last.portRead(io.readEn2DPE, io.rAddr2DPE, io.rData2DPE, "a")
    urams.last.portRead(io.readEn1DPE, io.rAddr1DPE, io.rData1DPE, "b")
  } otherwise { // read urams0, write urams1
    urams.last.portWrite(io.we, io.we, io.wAddr, io.wData, "a")
    urams.last.portClose("b")

    urams.head.portRead(io.readEn2DPE, io.rAddr2DPE, io.rData2DPE, "a")
    urams.head.portRead(io.readEn1DPE, io.rAddr1DPE, io.rData1DPE, "b")

  }

}
