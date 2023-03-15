package R2Plus1D
import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw._
import scala.util.Random.nextInt
class GlobalAvgPoolingTest extends org.scalatest.flatspec.AnyFlatSpec {

  "pooling " should "run in high freq" in MyVivadoAction(
    GlobalAvgPooling(uoc = 64, dataInWidth = 26, ofMapSize = 98, dataOutWidth = 8),
    "pooling",
    flowType = SYNTH
  )

  it should "work right" in {
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = GlobalAvgPooling(uoc = 64, dataInWidth = 8, ofMapSize = 98, dataOutWidth = 8)
        dut
      }
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(10)
        io.din.valid #= false
        io.din.last  #= false
        io.din.payload.foreach(_ #= 0)
        clockDomain.waitSampling()
        (0 until dut.ofMapSize).foreach { i =>
          io.din.valid #= true
          io.din.payload.foreach(_ #= (nextInt(127) - 60).toDouble)
          if (i == dut.ofMapSize - 1) {
            io.din.last #= true
          }
          clockDomain.waitSampling()
        }
        io.din.last  #= false
        io.din.valid #= false
        clockDomain.waitSampling(9)
        (0 until dut.ofMapSize).foreach { i =>
          io.din.valid #= true
          io.din.payload.foreach(_ #= (nextInt(127) - 60).toDouble)
          if (i == dut.ofMapSize - 1) {
            io.din.last #= true
          }
          clockDomain.waitSampling()
        }
        io.din.last  #= false
        io.din.valid #= false
        clockDomain.waitSampling(9)
      }
  }
}
