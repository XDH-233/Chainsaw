package Chainsaw.memory
import spinal.core._
import R2Plus1D._
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps

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
    val sdpuram: SDPURAM = SDPURAM(width = w, depth = d, pipeRegsCount = r)
    sdpuram.io.wea    := io.wea
    sdpuram.io.mem_en := io.mem_en
    sdpuram.io.dina   := io.dina
    sdpuram.io.addra  := io.addra
    sdpuram.io.addrb  := io.addrb
    io.doutb          := sdpuram.io.doutb
  }

  "output buffer 2D" should "consume proper resource" in MyVivadoAction(sdpURAM(w = 1024, d = 56648, r = 4), "output_buffer_2d", SYNTH)

  "output buffer 1D" should "consume proper resource" in MyVivadoAction(sdpURAM(w = 1024, d = 25088, r = 4), "output_buffer_1d", SYNTH)

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = sdpURAM(72, 1024 * 4, 4)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.wea    #= false
      io.mem_en #= false
      io.addra  #= 0
      io.addrb  #= 0
      clockDomain.waitSampling()
      io.mem_en #= true
      (0 until 10).foreach { i =>
        io.wea   #= true
        io.addra #= i
        io.dina.randomize()
        clockDomain.waitSampling()
      }
      io.wea #= false
      (0 until 10).foreach { i =>
        io.addra #= i
        clockDomain.waitSampling()
      }

    }
}
