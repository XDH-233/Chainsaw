package R2Plus1D

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

class AdderTreeDemoTest extends org.scalatest.flatspec.AnyFlatSpec {
  "adderTree64" should "work normally" in VivadoSynth(AdderTreeDemo(64), "sum64")

  "adderTree sim" should "work normally" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = AdderTreeDemo(64)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.a.foreach(_ #= 0)
      clockDomain.waitSampling()
      (0 until 100).foreach { _ =>
        io.a.foreach(_ #= scala.util.Random.nextInt(10))
        clockDomain.waitSampling()
      }
    }
}
