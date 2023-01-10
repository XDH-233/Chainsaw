package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class LoopInsideTileTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = LoopInsideTile2D()
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.loadConfig #= false
      io.Tc         #= 0
      io.Nihw       #= 0
      io.Nd         #= 0
      io.Nic        #= 0
      io.Krs        #= 0
      clockDomain.waitSampling()
      io.loadConfig  #= true
      io.Tc          #= 144
      io.Nd          #= 16
      io.Nihw        #= 56
      io.Nohw        #= 56
      io.Nic         #= 64
      io.Krs         #= 3
      io.Stride      #= true
      io.paddingSize #= 1
      clockDomain.waitSampling()
      io.loadConfig #= false
      clockDomain.waitSampling(10)
    }

}
