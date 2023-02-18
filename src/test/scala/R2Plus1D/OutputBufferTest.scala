package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class OutputBufferTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right " in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = OutputBuffer(uc = 2)

      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.we       #= false
      io.wAddr    #= 0
      io.wData    #= 0
      io.rAddrVld #= false
      io.rdEn     #= false
      io.rAddr    #= 0
      clockDomain.waitSampling()
      (0 until 10).foreach { i =>
        io.wAddr #= i
        io.wData.randomize()
        io.we #= true
        clockDomain.waitSampling()
      }
      io.we       #= false
      io.rdEn     #= true
      io.rAddrVld #= true
      (0 until 10).foreach { i =>
        io.rAddr #= i
        clockDomain.waitSampling()
      }
    }
}
