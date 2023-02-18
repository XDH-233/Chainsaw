package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class SimpleDualPortRAMTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = SimpleDualPortRAM(width = 8, depth = 16, pipeRegCount = 2)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.we    #= false
      io.en    #= false
      io.rAddr #= 0
      io.wAddr #= 0
      clockDomain.waitSampling()
      io.en      #= true
      (0 until 1 << log2Up(dut.depth)).foreach { i =>
        io.rAddr #= i
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(2)
      io.we    #= true
      io.wAddr #= 4
      clockDomain.waitSampling()
      io.we #= false
      clockDomain.waitSampling()
      io.rAddr #= 4
      clockDomain.waitSampling(4)
    }

}
