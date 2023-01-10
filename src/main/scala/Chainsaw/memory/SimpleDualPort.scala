package Chainsaw.memory
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SimpleDualPort(width: Int, depth: Int) extends Bundle with IMasterSlave {
  val we, en:       Bool = Bool()
  val addr:         UInt = UInt(log2Up(depth) bits)
  val wData, rData: Bits = Bits(width bits)

  override def asMaster(): Unit = {
    out(we, en, addr, wData)
    in(rData)
  }

  override type RefOwnerType = this.type
}
