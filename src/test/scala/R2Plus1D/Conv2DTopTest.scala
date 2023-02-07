package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class Conv2DTopTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right" in {

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = Conv2DTop()
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)

      }
  }

}
