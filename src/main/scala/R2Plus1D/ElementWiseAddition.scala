package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.language.postfixOps
import Chainsaw._

case class ElementWiseAddition(
    dataWidth:   Int = 8,
    uoc:         Int = 36,
    dataWidth0D: Int = 28,
    depth:       Int = Parameter.featureMapDepth,
    readLatency: Int = 4
) extends Component {
  val io = new Bundle {
    val enable:     Bool      = in Bool ()
    val ofMapWe1D:  Bool      = in Bool ()
    val ofMapAddr:  UInt      = in UInt (log2Up(depth) bits)
    val accRAMDout: Vec[SInt] = in Vec (SInt(dataWidth0D bits), uoc)

    val buffer0DRdEn:  Bool      = out Bool ()
    val buffer0DRAddr: UInt      = out UInt (log2Up(depth) bits)
    val buffer0DRData: Vec[Bits] = in Vec (Bits(dataWidth bits), uoc)

    val buffer0DWe:    Bool = out Bool ()
    val buffer0DWData: Bits = out Bits (dataWidth * uoc bits)
    val buffer0DWAddr: UInt = out UInt (log2Up(depth) bits)
  }
  import Function._
  io.buffer0DRdEn  := io.ofMapWe1D & io.enable
  io.buffer0DRAddr := io.ofMapAddr
  val sums: Seq[SInt] = io.accRAMDout.d(readLatency).zip(io.buffer0DRData).map { case (acc, buf) => acc + buf.asSInt }
  io.buffer0DWe    := io.buffer0DRdEn.d(readLatency) & io.enable
  io.buffer0DWAddr := io.buffer0DRAddr.d(readLatency)
  io.buffer0DWData := sums.map(_.relu(dataWidth)).map(_.asBits).reverse.reduce(_ ## _)
}
