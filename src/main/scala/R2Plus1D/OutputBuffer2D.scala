package R2Plus1D
import spinal.core._

import scala.language.postfixOps
import Chainsaw._
import Chainsaw.memory._

case class OutputBuffer2D(dataWidth: Int = 8, uc: Int = Parameter.Uc, readLatency: Int = 4, depth: Int = Parameter.outputBuffer2DDepth) extends Component {
  val io = new Bundle {
    val we    = in Bool ()
    val wAddr = in UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val wData = in Bits (dataWidth * uc bits)

    val rdEn     = in Bool ()
    val rAddr    = in UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val rAddrVld = in Bool ()
    val rData    = out Vec (Bits(dataWidth bits), uc)
  }

  val ram: SDPURAM = SDPURAM(width = dataWidth * uc, depth = depth)
  ram.io.mem_en := io.we | io.rdEn
  ram.io.addra  := io.wAddr
  ram.io.addrb  := io.rAddr
  ram.io.wea    := io.we
  ram.io.dina   := io.wData
  val rData = Bits(dataWidth * uc bits)

  rData := Mux(io.rAddrVld.d(readLatency), ram.io.doutb, B(0))
  Function.vecZip(io.rData, rData.subdivideIn(dataWidth bits))
}
