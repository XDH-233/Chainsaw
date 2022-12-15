package R2Plus1D

import Chainsaw.xilinx.VivadoSynth

case class InputBufferTest() extends org.scalatest.flatspec.AnyFlatSpec {
  "input buffer" should "consume 104 URAM" in VivadoSynth(InputBufer(), "input_buffer")
}
