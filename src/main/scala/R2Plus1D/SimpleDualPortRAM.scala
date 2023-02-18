package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.language.postfixOps
import Chainsaw._

case class SimpleDualPortRAM(width: Int, depth: Int, pipeRegCount: Int = 1) extends Component {
  val io = new Bundle {
    val en, we, rdEn = in Bool ()
    val wAddr, rAddr = in UInt (log2Up(depth) bits)
    val wData        = in Bits (width bits)
    val rData        = out Bits (width bits)
  }
  val content = Seq.fill(1 << log2Up(depth))(B(0))
  val mem     = Mem(Bits(width bits), content)
  mem.addAttribute("ram_style", "block")
  mem.write(io.wAddr, io.wData, io.en & io.we)
  io.rData := mem.readSync(io.rAddr, io.en).d(pipeRegCount)
}
