package R2Plus1D

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import Chainsaw.xilinx._

import scala.language.postfixOps
import scala.util.Random.nextInt

case class PETest() extends AnyFlatSpec {

  "PE 2D" should "consume 4608 DSPs" in MyVivadoAction(PE(uic = 64, uoc = 128), "PE", SYNTH)

  "PE 1D" should "consume 4608 DSPs" in MyVivadoAction(PE(uic = 128, uoc = 64), "PE", IMPL)

  "PE sim" should "work normally" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut: PE = PE(uic = 128, uoc = 64)
      dut
    }
    .doSim { dut: PE =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.ifMap.foreach((_: UInt) #= 0)
      io.weight.foreach((_: UInt) #= 0)
      clockDomain.waitSampling()
      (0 until 100).foreach { _: Int =>
        io.ifMap.foreach((_: UInt) #= nextInt(10))
        io.weight.foreach((_: UInt) #= nextInt(10))
        clockDomain.waitSampling()
      }
      println(dut.PELatency)
    }

}
