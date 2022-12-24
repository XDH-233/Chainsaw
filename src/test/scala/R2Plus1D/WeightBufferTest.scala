package R2Plus1D
import spinal.core._
import spinal.core.sim._
import Chainsaw.xilinx._
import scala.language.postfixOps

class WeightBufferTest extends org.scalatest.flatspec.AnyFlatSpec {

  "weight buffer 2D" should "consume proper resource" in MyVivadoAction(
    WeightBuffer(dataWidth = 512, depth = 20736, readLatency = 4),
    "weight_buffer_2D",
    SYNTH
  )

  "weight buffer 1D" should "consume proper resource" in MyVivadoAction(
    WeightBuffer(dataWidth = 1024, depth = 6912, readLatency = 4),
    "weight_buffer_1D",
    SYNTH
  )

  it should "sim right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = WeightBuffer(dataWidth = 16, depth = 256, readLatency = 4)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.switch    #= false
      io.writeEn   #= false
      io.writeData #= 0
      io.writeAddr #= 0

      io.readEn   #= false
      io.readAddr #= 0
      clockDomain.waitSampling()
      // write
      (0 until 10).foreach { i =>
        io.writeEn   #= true
        io.writeAddr #= i
        io.writeData.randomize()
        clockDomain.waitSampling()
      }
      io.switch  #= true
      io.writeEn #= false
      clockDomain.waitSampling()
      io.switch #= false
      clockDomain.waitSampling()
      // read and write
      (0 until 10).foreach { i =>
        io.writeEn   #= true
        io.writeAddr #= i
        io.writeData.randomize()
        io.readEn   #= true
        io.readAddr #= i
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(4)
      io.switch  #= true
      io.writeEn #= false
      clockDomain.waitSampling()
      io.switch #= false
      clockDomain.waitSampling()
      // read
      (0 until 10).foreach { i =>
        io.readEn   #= true
        io.readAddr #= i
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(10)
    }
}