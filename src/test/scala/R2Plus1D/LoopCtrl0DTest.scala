package R2Plus1D
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Chainsaw._
import Chainsaw.xilinx._
import R2Plus1D.model._

class LoopCtrl0DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right " in {
    val config = ConvConfig(Uic = 2, Nic = 3, Uoc = 2, Noc = 5, Nihw = 4, Nid = 2, convType = ConvType.D0)
    val conv0D = Conv0D(config)
    val ifMap  = conv0D.randomIfMap
    val weight = conv0D.randomWeight
    conv0D.loopUnroll(conv0D.ifMap2Mem(ifMap), conv0D.weight2Mem(weight))

    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = LoopCtrl0D()
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.loadConfig   #= false
        io.weightRdy    #= false
        io.weightFilled #= false
        io.config.assignConfig(config)
        clockDomain.waitSampling()
        io.loadConfig #= true
        clockDomain.waitSampling()
        io.loadConfig #= false
        clockDomain.waitSampling(8)
        io.weightFilled #= true
        clockDomain.waitSampling(200)

      }

  }
}
