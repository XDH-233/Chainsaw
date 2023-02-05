package R2Plus1D

import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class GeneralCounterTest extends org.scalatest.flatspec.AnyFlatSpec {
  it should "work right" in {
    case class Acc() extends Component {
      val io = new Bundle {
        val load:  Bool = in Bool ()
        val inc:   Bool = in Bool ()
        val value: UInt = out UInt (32 bits)
      }

      val acc: GeneralCounter = GeneralCounter()

      acc.load(io.load, U(1), U(28))
      when(io.inc) {
        acc.inc()
      }
      io.value := acc.value
    }

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Acc()
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.inc  #= false
        io.load #= false
        clockDomain.waitSampling()
        io.load #= true
        clockDomain.waitSampling()
        io.load #= false
        clockDomain.waitSampling()
        clockDomain.waitSampling()
        (0 until 1000).foreach { _ =>
          io.inc.randomize()
          clockDomain.waitSampling()
        }
      }

  }
}
