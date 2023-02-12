package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._
import scala.util.Random.nextInt

class AccRAM2DTest extends org.scalatest.flatspec.AnyFlatSpec {
  "accRam2D" should "use BRAM" in MyVivadoAction(AccRAM2D(), "acc_ram_2D", SYNTH)

  it should "work right " in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = AccRAM2D(dataWidth = 8, uoc = 10)
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)
      io.doutEn   #= false
      io.writeAcc #= false
      io.wData.foreach(_ #= 0)
      io.addr #= 0
      clockDomain.waitSampling()
      (0 until 8).foreach { i =>
        io.writeAcc #= true
        io.addr     #= i
        io.wData.foreach(_ #= nextInt(20) - 10)
        clockDomain.waitSampling()
      }
      io.doutEn #= true
      (0 until 8).foreach { i =>
        io.writeAcc #= true
        io.addr     #= i
        io.wData.foreach(_ #= nextInt(20) - 10)
        clockDomain.waitSampling()
      }
      io.writeAcc #= false
      clockDomain.waitSampling(5)
    }
}
