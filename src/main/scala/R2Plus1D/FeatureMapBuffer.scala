package R2Plus1D
import spinal.core._
import Chainsaw.memory._
import Chainsaw._
import spinal.lib._

import scala.language.postfixOps

case class FeatureMapBuffer(width: Int = 512, depth: Int = 50176, uic: Int = 36, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val switch: Bool = in Bool ()
    val we:     Bool = in Bool ()
    val wData:  Bits = in Bits (width * uic bits)
    val wAddr:  UInt = in UInt (log2Up(depth) bits)

    val readEn2DPE:   Bool = in Bool ()
    val readEn1DPE:   Bool = in Bool ()
    val rAddr2DPE:    UInt = in UInt (log2Up(depth) bits)
    val rAddr2DPEVld: Bool = in Bool ()
    val rAddr1DPE:    UInt = in UInt (log2Up(depth) bits)

    val rData2DPE: Vec[Bits] = out Vec (Bits(width bits), uic)
    val rData1DPE: Bits      = out Bits (width * uic bits)
  }

  val readDta2DPERAM: Bits = Bits(width * uic bits)

  val urams: Seq[TDPURAM] = Seq.fill(2)(TDPURAM(width = width * uic, depth = depth))

  val state: Bool = RegInit(False) // 0 -> read urams0, write urams1
  state.toggleWhen(io.switch)

  when(state) { // read urams1, write urams0
    urams.head.portWrite(io.we, io.we, io.wAddr, io.wData, "a")
    urams.head.portClose("b")

    urams.last.portRead(io.readEn2DPE, io.rAddr2DPE, readDta2DPERAM, "a")
    urams.last.portRead(io.readEn1DPE, io.rAddr1DPE, io.rData1DPE, "b")
  } otherwise { // read urams0, write urams1
    urams.last.portWrite(io.we, io.we, io.wAddr, io.wData, "a")
    urams.last.portClose("b")

    urams.head.portRead(io.readEn2DPE, io.rAddr2DPE, readDta2DPERAM, "a")
    urams.head.portRead(io.readEn1DPE, io.rAddr1DPE, io.rData1DPE, "b")
  }

  io.rData2DPE.zip(readDta2DPERAM.subdivideIn(width bits)).foreach { case (i, r) => i := Mux(io.rAddr2DPEVld, r, B(0)) }
}
