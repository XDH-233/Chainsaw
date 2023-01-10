package R2Plus1D

import spinal.core._
import Chainsaw.memory._

import scala.language.postfixOps

case class WeightBuffer(dataWidth: Int, depth: Int, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val switch:    Bool = in Bool ()
    val writeEn:   Bool = in Bool ()
    val writeAddr: UInt = in UInt (log2Up(depth) bits)
    val writeData: Bits = in Bits (dataWidth bits)

    val readEn:   Bool = in Bool ()
    val readAddr: UInt = in UInt (log2Up(depth) bits)
    val readData: Bits = out Bits (dataWidth bits)
  }

  val urams: Seq[SPURAM] = Seq.fill(2)(SPURAM(width = dataWidth, depth = depth, readLatency = readLatency, singlePortMode = ReadUnderWriteMode.NO_CHANGE))
  val state: Bool        = RegInit(False) // 0 -> read urams0, write uram1
  state.toggleWhen(io.switch)

  when(state) { // read urams1, write urams0
    urams.head.write(io.writeEn, io.writeEn, io.writeAddr, io.writeData)
    urams.last.read(io.readEn, io.readAddr, io.readData)
  } otherwise { // read urams0, write urams1
    urams.last.write(io.writeEn, io.writeEn, io.writeAddr, io.writeData)
    urams.head.read(io.readEn, io.readAddr, io.readData)
  }
}
