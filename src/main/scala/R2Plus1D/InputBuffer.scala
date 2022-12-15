package R2Plus1D

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.memory._

case class InputBufer(width: Int = 64 * 8, depth: Int = 49 * 1024, readLatency: Int = 4, w2rLatency: Int = 5) extends Component {
  val writeLatency = w2rLatency - readLatency

  val io = new Bundle {

    val we    = in Bits (2 bits) // write enable
    val memEn = in Bits (2 bits) // memory enable
    val din   = in Vec (Bits(width bits), 2) // data input
    val addr  = in Vec (UInt(log2Up(depth) bits), 2) // address input
    val dout  = out Vec (Bits(width bits), 2) // data output

  }

  val uram = tdp_uram(width, depth, readLatency)
  uram.io.wea     := io.we(0)
  uram.io.dina    := io.din(0)
  uram.io.addra   := io.addr(0)
  uram.io.mem_ena := io.memEn(0)
  io.dout(0)      := uram.io.douta

  uram.io.web     := io.we(1)
  uram.io.dinb    := io.din(1)
  uram.io.addrb   := io.addr(1)
  uram.io.mem_enb := io.memEn(1)
  io.dout(1)      := uram.io.doutb

}
