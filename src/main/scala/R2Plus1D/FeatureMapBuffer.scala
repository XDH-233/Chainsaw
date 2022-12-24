package R2Plus1D
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.memory._

import scala.language.postfixOps

case class FeatureMapBuffer(width: Int = 512, depth: Int = 50176, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val switch:      Bool      = in Bool ()
    val writeEnable: Bool      = in Bool ()
    val readEnable:  Bool      = in Bool ()
    val wAddr:       Vec[UInt] = in Vec (UInt(log2Up(depth) bits), 2)
    val wData:       Vec[Bits] = in Vec (Bits((width bits)), 2)

    val readAddr2DPE: UInt = in UInt (log2Up(depth) bits)
    val readAddr1DPE: UInt = in UInt (log2Up(depth) bits)

    val readData2DPE: Bits = out Bits (width bits)
    val readData1DPE: Bits = out Bits (width bits)
  }

  val urams: Seq[TDPURAM] = Seq.fill(2)(TDPURAM(width, depth, readLatency))

  val state: Bool = RegInit(False) // 0 -> read uram0, write uram1; 1 -> read uram1, write uram0
  state.toggleWhen(io.switch)

  // write data
  urams.foreach { u =>
    u.io.dina := io.wData(0)
    u.io.dinb := io.wData(1)
  }

  // read data MUX
  io.readData2DPE := Mux(state, urams(1).io.douta, urams.head.io.douta)
  io.readData1DPE := Mux(state, urams(1).io.doutb, urams.head.io.doutb)

  // address and write enable
  when(state) { // read uram1, write uram0
    urams(1).io.addra := io.readAddr2DPE
    urams(1).io.addrb := io.readAddr1DPE
    urams(1).io.wea.clear()
    urams(1).io.web.clear()
    urams(1).io.mem_ena := io.readEnable
    urams(1).io.mem_enb := io.readEnable

    urams.head.io.wea     := io.writeEnable
    urams.head.io.web     := io.writeEnable
    urams.head.io.mem_ena := io.writeEnable
    urams.head.io.mem_enb := io.writeEnable
    urams.head.io.dina    := io.wData(0)
    urams.head.io.dinb    := io.wData(1)
    urams.head.io.addra   := io.wAddr(0)
    urams.head.io.addrb   := io.wAddr(1)
  } otherwise { // write uram1
    urams.head.io.addra := io.readAddr2DPE
    urams.head.io.addrb := io.readAddr1DPE
    urams.head.io.wea.clear()
    urams.head.io.web.clear()
    urams.head.io.mem_ena := io.readEnable
    urams.head.io.mem_enb := io.readEnable

    urams(1).io.wea     := io.writeEnable
    urams(1).io.web     := io.writeEnable
    urams(1).io.mem_ena := io.writeEnable
    urams(1).io.mem_enb := io.writeEnable
    urams(1).io.dina    := io.wData(0)
    urams(1).io.dinb    := io.wData(1)
    urams(1).io.addra   := io.wAddr(0)
    urams(1).io.addrb   := io.wAddr(1)
  }
}
