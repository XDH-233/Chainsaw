package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps
import Chainsaw._

case class LoopCtrl0D(uic: Int = Parameter.Uic, uoc: Int = Parameter.Uoc) extends Component {
  val io = new Bundle {
    val loadConfig   = in Bool ()
    val weightRdy    = in Bool ()
    val weightFilled = in Bool ()
    val config       = in(ConfigParaPorts0D())
  }
  val ow    = GeneralCounter(step = U(1), top = io.config.Nohw, en = io.loadConfig)
  val oh    = GeneralCounter(step = U(1), top = io.config.Nohw, en = io.loadConfig)
  val ti    = GeneralCounter(step = U(1), top = io.config.NicDUicCeil, en = io.loadConfig)
  val tiNic = GeneralCounter(step = U(uic), top = io.config.Nic, en = io.loadConfig)
  val od    = GeneralCounter(step = U(1), top = io.config.Nod, en = io.loadConfig)
  val to    = GeneralCounter(step = U(1), top = io.config.NocDUocCeil, en = io.loadConfig)
  val toNoc = GeneralCounter(step = U(uoc), top = io.config.Noc, en = io.loadConfig)
  when(io.weightFilled) {
    ow.inc()
  }
  when(ow.willOverFlow) {
    oh.inc()
  }
  when(oh.willOverFlow) {
    ti.inc()
    tiNic.inc()
  }
  when(ti.willOverFlow) {
    od.inc()
  }
  when(od.willOverFlow) {
    to.inc()
    toNoc.inc()
  }

}
