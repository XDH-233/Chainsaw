package R2Plus1D

import spinal.core._
import spinal.core.sim._
import Chainsaw.xilinx._
import scala.language.postfixOps

class PingPongRegsTest extends org.scalatest.flatspec.AnyFlatSpec {

  "ping-pong regs" should "have good timing" in MyVivadoAction(PingPongRegs(), "ping_pong_regs", SYNTH)

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = PingPongRegs(uic = 4, uoc = 4, width = 2)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.load     #= false
      io.weightIn #= 0
      io.switch   #= false
      clockDomain.waitSampling()
      io.switch #= true
      clockDomain.waitSampling()
      io.switch #= false
      clockDomain.waitSampling()
      (0 until 4).foreach { _ =>
        io.load #= true
        io.weightIn.randomize()
        clockDomain.waitSampling()
      }
      io.switch #= true
      io.load   #= false
      clockDomain.waitSampling()
      io.switch #= false
      clockDomain.waitSampling()
      (0 until 3).foreach { _ =>
        io.load #= true
        io.weightIn.randomize()
        clockDomain.waitSampling()
      }
      io.switch #= true
      io.load   #= false
      clockDomain.waitSampling()
      io.switch #= false
      clockDomain.waitSampling(100)
    }
}
