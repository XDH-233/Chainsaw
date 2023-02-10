package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class PingPongRegs1DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = PingPongRegs1D(uc = 2, uoc = 6)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.weightRdy      #= false
      io.loadConfig     #= false
      io.layerDone      #= false
      io.ofMapSize2D    #= 9
      io.weightLoadNum  #= 6
      io.weightAddrBase #= 0
      clockDomain.waitSampling()
      io.loadConfig #= true
      clockDomain.waitSampling()
      io.loadConfig #= false
      io.weightRdy  #= true
      (0 until 100).foreach { _ =>
        io.weightIn.randomize()
        clockDomain.waitSampling()
      }
    }
}
