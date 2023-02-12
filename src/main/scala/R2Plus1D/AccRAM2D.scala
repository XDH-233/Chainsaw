package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.language.postfixOps
import Chainsaw._

case class AccRAM2D(uoc: Int = Parameter.Uc, dataWidth: Int = 26, readLatency: Int = 2) extends Component {
  val io = new Bundle {
    val writeAcc    = in Bool ()
    val addr        = in UInt (log2Up(uoc) bits)
    val wData       = in Vec (SInt(dataWidth bits), uoc)
    val doutEn      = in Bool ()
    val dout        = out Bits (dataWidth * uoc bits)
    val doutObserve = out Vec (SInt(dataWidth bits), uoc)
  }
  val ram = SimpleDualPortRAM(width = dataWidth * uoc, depth = uoc, readLatency = readLatency)
  ram.io.en    := io.writeAcc | io.writeAcc.d(readLatency)
  ram.io.we    := io.writeAcc.d(readLatency)
  ram.io.rAddr := io.addr
  val readData: Seq[SInt] = ram.io.rData.subdivideIn(dataWidth bits).map(_.asSInt)
  val sums:     Seq[SInt] = readData.zip(io.wData.map(_.d(readLatency))).map { case (r, wd) => r + wd }
  ram.io.wAddr := io.addr.d(2)
  ram.io.wData := Mux(io.doutEn.d(readLatency), B(0), sums.map(_.asBits).reverse.reduce(_ ## _))
  io.dout      := Mux(io.doutEn.d(readLatency), sums.map(_.asBits).reverse.reduce(_ ## _), B(0))
  io.doutObserve.zip(sums).foreach { case (d, s) => d := Mux(io.doutEn.d(readLatency), s, S(0)) }
}
