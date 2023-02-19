package R2Plus1D
import spinal.core._
import Chainsaw.memory._
import Chainsaw._
import spinal.lib._

import scala.language.postfixOps

case class FeatureMapBuffer(width: Int = 512, depth: Int = 50176, uic: Int = 36, pipeRegCount: Int = 4) extends Component {
  val readLatency: Int = pipeRegCount + 1
  val io = new Bundle {
    val we:    Bool = in Bool ()
    val wData: Bits = in Bits (width * uic bits)
    val wAddr: UInt = in UInt (log2Up(depth) bits)

    val readEn2DPE:   Bool = in Bool ()
    val readEn1DPE:   Bool = in Bool ()
    val rAddr2DPE:    UInt = in UInt (log2Up(depth) bits)
    val rAddr2DPEVld: Bool = in Bool ()
    val rAddr1DPE:    UInt = in UInt (log2Up(depth) bits)
    val rAddr1DPEVld: Bool = in Bool ()

    val rData2DPE: Vec[Bits] = out Vec (Bits(width bits), uic)
    val rData1DPE: Vec[Bits] = out Vec (Bits(width bits), uic)
  }

  val readData2DPERAM: Bits = Bits(width * uic bits)
  val readData1DPERAM: Bits = Bits((width * uic bits))

  val uram: TDPURAM = TDPURAM(width = width * uic, depth = depth, pipeRegCount = pipeRegCount)

  uram.io.dina    := io.wData
  uram.io.addra   := Mux(io.we, io.wAddr, io.rAddr2DPE)
  uram.io.wea     := io.we
  uram.io.mem_ena := io.we | io.readEn2DPE

  uram.io.dinb.clearAll()
  uram.io.addrb := io.rAddr1DPE
  uram.io.web.clear()
  uram.io.mem_enb := io.readEn1DPE

  readData2DPERAM := uram.io.douta
  readData1DPERAM := uram.io.doutb

  io.rData2DPE.zip(readData2DPERAM.subdivideIn(width bits)).foreach { case (i, r) => i := Mux(io.rAddr2DPEVld.d(readLatency), r, B(0)) }
  io.rData1DPE.zip(readData1DPERAM.subdivideIn(width bits)).foreach({ case (o, r) => o := Mux(io.rAddr1DPEVld.d(readLatency), r, B(0)) })
}
