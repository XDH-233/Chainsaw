package Chainsaw.memory
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._
import Chainsaw._

class TDPURAMTest extends org.scalatest.flatspec.AnyFlatSpec {

  "tdp uram syth" should "behave right pipeline after uram read" in R2Plus1D.MyVivadoAction(tdpURAM(288, 152 * 1024, 4), "tdp_uram", IMPL)

  it should "right read latency" in {

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = tdpURAM(8, 4096, 4)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.dina #= 0
        io.dinb #= 0

        io.mem_ena #= false
        io.mem_enb #= false

        io.addra #= 0
        io.addrb #= 0

        io.wea #= false
        io.web #= false
        clockDomain.waitSampling()
        (0 until 10).foreach { t =>
          io.mem_ena #= true
          io.wea     #= true
          io.addra   #= t
          io.dina.randomize()
          clockDomain.waitSampling()
        }
        io.wea     #= false
        io.mem_ena #= false
        clockDomain.waitSampling()
        (0 until 10).foreach { t =>
          io.mem_enb #= true
          io.addrb   #= t
          clockDomain.waitSampling()
        }
        clockDomain.waitSampling(5)
      }
  }
}

case class tdpURAM(w: Int, d: Int, r: Int, writeLatency: Int = 2) extends Component {
  val io = new Bundle {
    val wea:     Bool = in Bool () // Write Enable
    val mem_ena: Bool = in Bool () // Memory Enable
    val dina:    Bits = in Bits (w bits) // Data Input
    val addra:   UInt = in UInt (log2Up(d) bits) // Address Input
    val douta:   Bits = out Bits (w bits) // Data Output

    // Port B
    val web:     Bool = in Bool () // Write Enable
    val mem_enb: Bool = in Bool () // Memory Enable
    val dinb:    Bits = in Bits (w bits) // Data Input
    val addrb:   UInt = in UInt (log2Up(d) bits) // Address Input
    val doutb:   Bits = out Bits (w bits) // Data Output

  }

  val tdpURAMBlackBOx: TDPURAM = TDPURAM(w, d, r)

  tdpURAMBlackBOx.io.wea := io.wea
  tdpURAMBlackBOx.io.web := io.web

  tdpURAMBlackBOx.io.dina := io.dina
  tdpURAMBlackBOx.io.dinb := io.dinb

  tdpURAMBlackBOx.io.addra := io.addra
  tdpURAMBlackBOx.io.addrb := io.addrb

  tdpURAMBlackBOx.io.mem_ena := io.mem_ena
  tdpURAMBlackBOx.io.mem_enb := io.mem_enb

  io.douta := tdpURAMBlackBOx.io.douta
  io.doutb := tdpURAMBlackBOx.io.doutb
}
