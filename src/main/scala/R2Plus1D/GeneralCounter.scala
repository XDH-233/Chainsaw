package R2Plus1D
import spinal.core._

import scala.language.postfixOps

case class GeneralCounter(valueWidth: Int = 32, stepWidth: Int = 16, topWidth: Int = 32) extends ImplicitArea[UInt] {

  val valueNext: UInt = UInt(valueWidth bits)
  val value:     UInt = RegNext(valueNext) init (0)
  val step:      UInt = Reg(UInt(stepWidth bits))
  val top:       UInt = Reg(UInt(topWidth bits))

  val willOverFlowIfInc: Bool = value + step >= top // [0, top) 每次加step
  val willInc:           Bool = False.allowOverride
  val willOverFlow:      Bool = willOverFlowIfInc & willInc
  private val willLoad:  Bool = False.allowOverride
  private val willClear: Bool = False.allowOverride

  def load(en: Bool, s: UInt, t: UInt): Unit = {
    when(en) {
      willLoad.set()
      step := s.resized
      top  := t.resized
    }
  }

  def inc(): Unit = willInc.set()

  def clear(): Unit = willClear.set()

  when(willLoad | willClear) {
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

object GeneralCounter {
  def apply(step: UInt, top: UInt, en: Bool): GeneralCounter = {
    val acc = GeneralCounter()
    acc.load(en, step, top)
    acc
  }
}
