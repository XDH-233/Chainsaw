package R2Plus1D

import spinal.core._
import spinal.core.sim._
import Chainsaw.xilinx._
import scala.language.postfixOps

class PingPongRegs2DTest extends org.scalatest.flatspec.AnyFlatSpec {

  "ping-pong regs" should "have good timing" in MyVivadoAction(PingPongRegs2D(), "ping_pong_regs", SYNTH)

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = PingPongRegs2D(uic = 4, uoc = 12, width = 2)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.weightBufferRdy #= false
      io.weightAddrBase  #= 0
      io.weightIn        #= 0
      io.weightLoadedNum #= 0
      clockDomain.waitSampling()
      io.weightLoadedNum #= 12
      io.weightBufferRdy #= true
      clockDomain.waitSampling()
      (0 until 48).foreach { _ =>
        io.weightIn.randomize()
        clockDomain.waitSampling()
      }
      io.weightLoadedNum #= 8
      clockDomain.waitSampling()
      (0 until 47).foreach { _ =>
        io.weightIn.randomize()
        clockDomain.waitSampling()
      }
      io.weightLoadedNum #= 3
      clockDomain.waitSampling()
      (0 until 50).foreach { _ =>
        io.weightIn.randomize()
        clockDomain.waitSampling()
      }
    }

}
