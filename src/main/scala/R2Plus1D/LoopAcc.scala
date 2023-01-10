package R2Plus1D
import spinal.core._

import scala.language.postfixOps

case class LoopAcc(valueWidth: Int = 32, stepWidth: Int = 16, topWidth: Int = 32) extends ImplicitArea[UInt] {

  val valueNext: UInt = UInt(valueWidth bits)
  val value:     UInt = RegNext(valueNext)
  val step:      UInt = Reg(UInt(stepWidth bits))
  val top:       UInt = Reg(UInt(topWidth bits))

  val willOverFlowIfInc: Bool = value + step >= top
  val willInc:           Bool = False.allowOverride
  val willOverFlow:      Bool = willOverFlowIfInc & willInc
  val willLoad:          Bool = False.allowOverride

  def load(en: Bool, s: UInt, t: UInt): Unit = {
    when(en) {
      willLoad.set()
      step := s
      top  := t
    }
  }

  def inc(): Unit = willInc.set()

  when(willLoad) {
    valueNext.clearAll()
  } elsewhen (willInc) {
    when(willOverFlowIfInc) {
      valueNext.clearAll()
    } otherwise {
      valueNext := value + step
    }
  } otherwise {
    valueNext := value
  }

  override def implicitValue: UInt = this.value

  override type RefOwnerType = this.type
}

object LoopAcc {
  def apply(step: UInt, top: UInt, value: UInt): LoopAcc = {
    val acc = LoopAcc()
    value    := acc.value
    acc.step := step
    acc.top  := top
    acc
  }
}
