package R2Plus1D

import spinal.core._
import Chainsaw._

import scala.language.postfixOps

case class Multiplier(width: Int = 8, latency: Int = 2) extends Module {
  val io = new Bundle {
    val a, b, c: SInt      = in SInt (width bits) // p0 = a * b, p1 = a * c
    val p:       Vec[SInt] = out Vec (SInt(width * 2 bits), 2)
  }

  val aAbs:    UInt = abs(io.a)
  val bAbs:    UInt = abs(io.b)
  val cAbs:    UInt = abs(io.c)
  val concat:  UInt = (bAbs << (width * 2)) + cAbs
  val product: UInt = (aAbs * concat).d(latency) addAttribute ("use_dsp", "yes")

  val aPbAbs: UInt = product.takeHigh(width * 2).asUInt
  val aPcAbs: UInt = product.takeLow(width * 2).asUInt

  val p0Sign: Bool = (io.a.sign ^ io.b.sign).d(latency)
  val p1Sign: Bool = (io.a.sign ^ io.c.sign).d(latency)

  io.p(0) := complement(p0Sign, aPbAbs)
  io.p(1) := complement(p1Sign, aPcAbs)

  def abs(s: SInt):                    UInt = Mux(s.sign, (~(s - 1)).asUInt, s.asUInt)
  def complement(sign: Bool, u: UInt): SInt = Mux(sign, (~u + 1).asSInt, u.asSInt)
}

object Multiplier {
  def apply(a: SInt, b: SInt, c: SInt, latency: Int): Multiplier = {
    val m: Multiplier = Multiplier(width = a.getWidth, latency = latency)
    m.io.a := a
    m.io.b := b
    m.io.c := c
    m
  }
}
