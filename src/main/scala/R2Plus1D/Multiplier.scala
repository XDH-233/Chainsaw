package R2Plus1D

import spinal.core._
import Chainsaw._

import scala.language.postfixOps

case class Multiplier(width: Int = 8, latency: Int = 2) extends Module {
  val io = new Bundle {
    val a, b, c: UInt      = in UInt (width bits) // p0 = a * b, p1 = a * c
    val p:       Vec[UInt] = out Vec (UInt(width * 2 bits), 2)

  }
  val concat:  UInt = (io.b << (width * 2)) + io.c
  val product: UInt = RegNext(io.a * concat)

  val productReg: UInt = product.d(latency - 1)

  io.p(0) := productReg.takeHigh(width * 2).asUInt
  io.p(1) := productReg.takeLow(width * 2).asUInt
}

object Multiplier {
  def apply(a: UInt, b: UInt, c: UInt, latency: Int): Multiplier = {
    val m: Multiplier = Multiplier(width = a.getWidth, latency = latency)
    m.io.a := a
    m.io.b := b
    m.io.c := c
    m
  }
}
