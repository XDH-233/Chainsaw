package R2Plus1D

import Chainsaw.xilinx.{VivadoImpl, VivadoSynth}
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random.nextInt

case class PETest() extends AnyFlatSpec {

  "PE" should "consume 4608 DSPs" in VivadoImpl(PE(uic = 64, uoc = 144), "PE")

  "PE sim" should "work normally" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut: PE = PE(uic = 64, uoc = 114)
      dut
    }
    .doSim { dut: PE =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.ifMap.foreach((_: UInt)  #= 0)
      io.weight.foreach((_: UInt) #= 0)
      io.clr                      #= false
      io.enable                   #= false
      clockDomain.waitSampling()
      (0 until 100).foreach { _: Int =>
        io.clr #= nextInt(10) == 5
        io.enable.randomize()
        io.ifMap.foreach((_: UInt)  #= nextInt(10))
        io.weight.foreach((_: UInt) #= nextInt(10))
        clockDomain.waitSampling()
      }
    }

}
