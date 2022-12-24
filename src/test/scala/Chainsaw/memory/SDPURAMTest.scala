package Chainsaw.memory
import spinal.core._
import R2Plus1D._
import Chainsaw.xilinx._

import scala.language.postfixOps

class SDPURAMTest extends org.scalatest.flatspec.AnyFlatSpec {

  case class sdpURAM(w: Int, d: Int, r: Int) extends Component {
    val io = new Bundle {
      val wea:    Bool = in Bool ()
      val mem_en: Bool = in Bool ()
      val dina:   Bits = in Bits (w bits)
      val addra:  UInt = in UInt (log2Up(d) bits)
      val addrb:  UInt = in UInt (log2Up(d) bits)
      val doutb:  Bits = out Bits (w bits)
    }
    val sdpuram: SDPURAM = SDPURAM(width = w, depth = d, readLatency = r)
    sdpuram.io.wea    := io.wea
    sdpuram.io.mem_en := io.mem_en
    sdpuram.io.dina   := io.dina
    sdpuram.io.addra  := io.addra
    sdpuram.io.addrb  := io.addrb
    io.doutb          := sdpuram.io.doutb
  }

  "output buffer 2D" should "consume proper resource" in MyVivadoAction(sdpURAM(w = 1024, d = 56648, r = 4), "output_buffer_2d", SYNTH)

  "output buffer 1D" should "consume proper resource" in MyVivadoAction(sdpURAM(w = 1024, d = 25088, r = 4), "output_buffer_1d", SYNTH)

}
