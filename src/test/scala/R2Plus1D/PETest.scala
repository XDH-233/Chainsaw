package R2Plus1D

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import Chainsaw.xilinx._

import scala.language.postfixOps
import scala.util.Random.nextInt

case class PETest() extends AnyFlatSpec {

  "PE 2D" should "consume 2592 DSPs" in MyVivadoAction(PE(uic = 36, uoc = 144), "PE", SYNTH)

  "PE 1D" should "consume 2592 DSPs" in MyVivadoAction(PE(uic = 144, uoc = 36), "PE", IMPL)

  "PE sim" should "work normally" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut: PE = PE(uic = 36, uoc = 144)
      dut
    }
    .doSim { dut: PE =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.ifMap.foreach((_: SInt) #= 0)
      io.weight.foreach((_: SInt) #= 0)
      clockDomain.waitSampling()
      val count   = 100
      val ifMaps  = Array.tabulate(count, uic)((_, _) => nextInt(1 << width) - (1 << (width - 1)))
      val weights = Array.tabulate(count, uic * uoc)((i, j) => nextInt(1 << width) - (1 << (width - 1)))

      (0 until dut.PELatency).foreach { i =>
        io.ifMap.zip(ifMaps(i)).foreach { case (p, d) => p #= d }
        io.weight.zip(weights(i)).foreach { case (p, d) => p #= d }
        clockDomain.waitSampling()
      }
      (PELatency until PELatency + count).foreach { i =>
        if (i < count) {
          io.ifMap.zip(ifMaps(i)).foreach { case (p, d) => p #= d }
          io.weight.zip(weights(i)).foreach { case (p, d) => p #= d }
        }
        clockDomain.waitSampling()
        val golden = weights(i - PELatency).grouped(uic).toArray.map(w => w.zip(ifMaps(i - PELatency)).map({ case (wi, fi) => wi * fi }).sum)
        io.partialSum.zip(golden).foreach { case (p, g) => assert(p.toInt == g) }
        println("golden: " + golden.mkString(" "))
        println("sum:    " + io.partialSum.map(_.toInt).mkString(" "))
      }

    }

}
