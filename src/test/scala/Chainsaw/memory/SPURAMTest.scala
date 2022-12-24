package Chainsaw.memory
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class SPURAMTest extends org.scalatest.flatspec.AnyFlatSpec {

  "sp uram no change" should "work in mode of no change" in sim(ReadUnderWriteMode.NO_CHANGE)

  "sp uram read first" should "work in mode of read first" in sim(ReadUnderWriteMode.READ_FIRST)

  "sp uram write first" should "work in mode of write first" in sim(ReadUnderWriteMode.WRITE_FIRST)

  case class spURAM(width: Int, depth: Int, readLatency: Int, singlePortMode: ReadUnderWriteMode.Value) extends Component {
    val io = new Bundle {
      val we:     Bool = in Bool () // Write Enable
      val mem_en: Bool = in Bool () // Memory Enable
      val din:    Bits = in Bits (width bits) // Data Input
      val addr:   UInt = in UInt (log2Up(depth) bits) // Address Input
      val dout:   Bits = out Bits (width bits) // Data Output
    }

    val spuram: SPURAM = SPURAM(width, depth, readLatency, singlePortMode)

    spuram.io.we     := io.we
    spuram.io.mem_en := io.mem_en
    spuram.io.din    := io.din
    spuram.io.addr   := io.addr
    io.dout          := spuram.io.dout
  }

  def sim(singlePortMode: ReadUnderWriteMode.Value): Unit = {
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = spURAM(16, 4096, 4, singlePortMode)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.we     #= false
        io.mem_en #= false
        io.din    #= 0
        io.addr   #= 0
        clockDomain.waitSampling()
        (0 until 10).foreach { i =>
          io.mem_en #= true
          io.addr   #= i
          io.we     #= true
          io.din    #= i
          clockDomain.waitSampling()
        }
        (0 until 10).foreach { i =>
          io.we   #= false
          io.addr #= i
          clockDomain.waitSampling()
        }
        io.we   #= true
        io.addr #= 0
        io.din  #= 100
        clockDomain.waitSampling()
        io.we #= false
        clockDomain.waitSampling(100)

      }
  }

}
