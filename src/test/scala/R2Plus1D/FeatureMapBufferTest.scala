package R2Plus1D

import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import Chainsaw.xilinx._

class FeatureMapBufferTest extends org.scalatest.flatspec.AnyFlatSpec {


  "feature map buffer" should "consume right resource" in MyVivadoAction(FeatureMapBuffer(), "feature_map_buffer", SYNTH)


 it should "sim well" in SimConfig.withFstWave
   .withConfig(
     SpinalConfig(
       defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
       defaultClockDomainFrequency  = FixedFrequency(100 MHz)
     )
   )
   .compile {
     val dut= FeatureMapBuffer()
     dut
   }
   .doSim { dut =>
     import dut._
     dut.clockDomain.forkStimulus(10)
      io.switch #= false
     io.writeEnable #= false
     io.readEnable #= false
     io.wData.foreach(_#=0)
     io.wAddr.foreach(_#=0)
     io.readAddr2DPE #=0
     io.readAddr1DPE #=0
     clockDomain.waitSampling()
    // write
     (0 until 10).foreach{t=>
       io.writeEnable #= true
       io.wAddr.zipWithIndex.foreach{case(a,i) => a #= t * 2 + i}
       io.wData.randomize()
       clockDomain.waitSampling()
       println(io.wData.map(_.toBigInt).mkString("\n"))
       println("---------------------")
     }
     println("*"*10)
     io.writeEnable #= false
     io.switch #= true
     io.wAddr.foreach(_#=0)
     clockDomain.waitSampling(1)
     io.switch #= false
     clockDomain.waitSampling(4)
     (0 until 10).foreach{t =>
       io.readEnable #= true
      io.readAddr2DPE #= t * 2
       io.readAddr1DPE #= t * 2 + 1
       clockDomain.waitSampling()
       println(io.readData2DPE.toBigInt)
       println(io.readData1DPE.toBigInt)
       println("--------------------")
     }
     (0 until 10).foreach{_=>
       clockDomain.waitSampling()
       println(io.readData2DPE.toBigInt)
       println(io.readData1DPE.toBigInt)
       println("--------------------")
     }
}

}
