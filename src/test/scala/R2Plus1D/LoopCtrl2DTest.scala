package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class LoopCtrl2DTest extends org.scalatest.flatspec.AnyFlatSpec {

  "loopCtrl2D" should "run in high frequency" in MyVivadoAction(LoopCtrl2D(), "loop_ctrl_2D", SYNTH)

  it should "work right" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = LoopCtrl2D()
      dut
    }
    .doSim { dut =>
      import dut._
      dut.clockDomain.forkStimulus(10)

      io.Nic         #= 64 // Uic = 36
      io.Nc          #= 380 // Uc = 144
      io.Nd          #= 3
      io.Nohw        #= 8
      io.Krs         #= 3
      io.Toh         #= 3
      io.Tow         #= 3
      io.loadConfig  #= false
      io.PEDone      #= false
      io.NcDUcCeil   #= 3
      io.NicDUicCeil #= 2
      io.kernelSize  #= 9
      clockDomain.waitSampling()
      io.loadConfig #= true
      clockDomain.waitSampling()
      io.loadConfig #= false
      clockDomain.waitSampling()
      io.PEDone #= true
      clockDomain.waitSampling(10000)
    }

}
