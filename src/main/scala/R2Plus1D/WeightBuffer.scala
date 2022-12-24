package R2Plus1D

import spinal.core._
import Chainsaw.memory._

import scala.language.postfixOps

case class WeightBuffer(dataWidth: Int, depth: Int, readLatency: Int) extends Component {
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

  urams.foreach(u => u.io.din := io.writeData)
  io.readData := Mux(state, urams.last.io.dout, urams.head.io.dout)

  when(state) { // read urams1
    // enable
    urams.head.io.mem_en := io.writeEn
    urams.head.io.we     := io.writeEn
    urams.last.io.mem_en := io.readEn
    urams.last.io.we.clear()
    // address
    urams.head.io.addr := io.writeAddr
    urams.last.io.addr := io.readAddr
  } otherwise {
    // enable
    urams.last.io.mem_en := io.writeEn
    urams.last.io.we     := io.writeEn
    urams.head.io.mem_en := io.readEn
    urams.head.io.we.clear()
    // address
    urams.last.io.addr := io.writeAddr
    urams.head.io.addr := io.readAddr
  }
}
