package R2Plus1D

import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._
import scala.util.Random.nextInt

class FeatureMapBufferTest extends org.scalatest.flatspec.AnyFlatSpec {

  "feature map buffer" should "consume right resource" in MyVivadoAction(
    FeatureMapBuffer(width = 36 * 8, depth = 25 * 4 * 1024),
    "feature_map_buffer",
    IMPL
  )

  it should "sim well" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = FeatureMapBuffer(width = 8)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.we    #= false
      io.wAddr #= 0
      io.wData #= 0

      io.readEn2DPE #= false
      io.readEn1DPE #= false
      io.rAddr2DPE  #= 0
      io.rAddr1DPE  #= 0
      io.switch     #= false
      clockDomain.waitSampling()

      (0 until 20).foreach { i =>
        io.we    #= true
        io.wAddr #= i
        io.wData.randomize()
        clockDomain.waitSampling()
      }
      io.we     #= false
      io.switch #= true
      clockDomain.waitSampling()
      io.switch #= false
      clockDomain.waitSampling()
      (0 until 10).foreach { i =>
        io.readEn2DPE #= true
        io.readEn1DPE #= true
        io.rAddr2DPE  #= i
        io.rAddr1DPE  #= i + 10
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(5)
    }

}
