package R2Plus1D
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps
import Chainsaw._

import scala.collection.mutable

class MultiplierTest extends org.scalatest.flatspec.AnyFlatSpec {

  "synth" should "use dsp and have high frequency" in MyVivadoAction(Multiplier(8, 2), "multiDSP", SYNTH)

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = Multiplier(8, 2)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.a #= 0
      io.b #= 0
      io.c #= 0
      clockDomain.waitSampling()
      val aQ  = mutable.Queue[Int]()
      val bcQ = mutable.Queue[Int]()
      aQ.enqueue(0)
      aQ.enqueue(0)
      bcQ.enqueue(0)
      bcQ.enqueue(0)
      for (a <- -128 to 127; bc <- -128 to 127) {
        io.a #= a
        io.b #= bc
        io.c #= bc
        clockDomain.waitSampling()
        aQ.enqueue(a)
        bcQ.enqueue(bc)
        val aL  = aQ.dequeue()
        val bcL = bcQ.dequeue()
        println(aL)
        println(bcL)
        println("p0 " + io.p(0).toInt)
        println("p1 " + io.p(1).toInt)
        println("------------")
        assert(io.p(0).toInt == aL * bcL)

      }

    }

}
