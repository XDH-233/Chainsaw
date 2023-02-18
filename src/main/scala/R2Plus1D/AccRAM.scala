package R2Plus1D
import spinal.core._
import scala.language.postfixOps
import Chainsaw._

case class AccRAM(uoc: Int = Parameter.Uc, dataWidth: Int = 26, pipeRegCount: Int = 2, dataOutWidth: Int = 8, depth: Int) extends Component {
  val readLatency: Int = pipeRegCount + 1
  val io = new Bundle {
    val writeAcc: Bool      = in Bool ()
    val addr:     UInt      = in UInt (log2Up(depth) bits)
    val wData:    Vec[SInt] = in Vec (SInt(dataWidth bits), uoc)
    val doutEn:   Bool      = in Bool ()
    val doutReLU: Bits      = out Bits (dataOutWidth * uoc bits)
    val douts:    Vec[SInt] = out Vec (SInt(dataWidth bits), uoc)
  }
  val ram: SimpleDualPortRAM = SimpleDualPortRAM(width = dataWidth * uoc, depth = depth, pipeRegCount = pipeRegCount)
  ram.io.en    := io.writeAcc | io.writeAcc.d(pipeRegCount)
  ram.io.we    := io.writeAcc.d(pipeRegCount)
  ram.io.rAddr := io.addr
  val readData: Seq[SInt] = ram.io.rData.subdivideIn(dataWidth bits).map(_.asSInt)
  val sums:     Seq[SInt] = readData.zip(io.wData.map(_.d(pipeRegCount))).map { case (r, wd) => r + wd }
  ram.io.wAddr := io.addr.d(pipeRegCount)
  ram.io.wData := Mux(io.doutEn.d(pipeRegCount), B(0), sums.map(_.asBits).reverse.reduce(_ ## _))
  import Function.SIntExtension
  val sumsReLU: Seq[SInt] = sums.map(s => s.relu(dataOutWidth))
  io.doutReLU := Mux(io.doutEn.d(pipeRegCount), sumsReLU.map(_.asBits).reverse.reduce(_ ## _), B(0))
  Function.vecZip(io.douts, Vec(sums))
}
