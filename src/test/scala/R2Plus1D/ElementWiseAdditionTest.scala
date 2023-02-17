package R2Plus1D

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._
import scala.util.Random.nextInt

class ElementWiseAdditionTest extends AnyFlatSpec {

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = ElementWiseAddition(uoc = 4)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.ofMapWe1D #= false
      io.ofMapAddr #= 0
      io.accRAMDout.foreach(_ #= 0)
      io.buffer0DRData.foreach(_ #= 0)
      clockDomain.waitSampling()

      (0 until 100).foreach { i =>
        io.ofMapWe1D #= true
        io.ofMapAddr #= i
        io.accRAMDout.foreach(_ #= nextInt(400) - 200)
        io.buffer0DRData.randomize()
        clockDomain.waitSampling()
      }
    }
}
