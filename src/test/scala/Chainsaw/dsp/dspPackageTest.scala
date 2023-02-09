package Chainsaw.dsp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus._
import spinal.lib.bus.regif._
import spinal.sim._
import spinal.core.sim._

import scala.language.postfixOps // for more simulation

class dspPackageTest extends org.scalatest.flatspec.AnyFlatSpec {

  val sin =
    (0 until 10000).map(_.toDouble).map(scala.math.sin).map(BigDecimal(_))
  val sin_shifted =
    (10 until 10010).map(_.toDouble).map(scala.math.sin).map(BigDecimal(_))

  "signal export and import" should "work" in {
    val file = exportSignal(sin, sin_shifted)
    println(importSignal(file).head.take(100).mkString(" "))
  }

  "plot_spectrum" should "work" in plotSpectrum(sin, 240 MHz)
  "get_corr" should "work" in corrMetric(sin, sin_shifted, 0.9)

}
