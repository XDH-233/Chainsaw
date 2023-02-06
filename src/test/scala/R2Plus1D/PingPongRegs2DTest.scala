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
      val dut = PingPongRegs2D(uic = 4, uoc = 10, width = 2)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.weightBufferRdy #= false
      io.weightAddrBase  #= 0
      io.tileDone        #= false
      io.weightIn        #= 0
      clockDomain.waitSampling()
      io.weightBufferRdy #= true
      io.weightLoadedNum #= 8
      clockDomain.waitSampling(5)
      (0 until 8).foreach { i =>
        io.weightIn #= i
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(4)
      (0 until 8).foreach { i =>
        io.weightIn #= 8 - i
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(10)
      io.tileDone #= true
      clockDomain.waitSampling()
      io.tileDone #= false
      io.weightIn #= 100
      clockDomain.waitSampling(30)
      io.tileDone #= true
      clockDomain.waitSampling()
      io.tileDone #= false
      io.weightIn #= 45
      clockDomain.waitSampling(30)
    }
}
