package R2Plus1D

import spinal.core._
import spinal.lib._
import Parameter._
import scala.math.sqrt
import Chainsaw._

import scala.language.postfixOps

case class LoopCtrl2D() extends Component {
  val width = 16
  val io = new Bundle {
    val Nic, Nc, Nohw, Nd, Krs, Tow, Toh       = in UInt (width bits)
    val NcDUcCeil, NicDUicCeil                 = in UInt (width bits)
    val kernelSize, ifMapSize                  = in UInt (width bits)
    val weightRdy, fMapRdy, PEDone, loadConfig = in Bool ()
  }

  val to:   GeneralCounter = GeneralCounter(step = U(1), top = io.NcDUcCeil, io.loadConfig)
  val toNc: GeneralCounter = GeneralCounter(step = U(Uc), top = io.Nc, io.loadConfig)

  val ti:    GeneralCounter = GeneralCounter(step = U(1), top = io.NicDUicCeil, io.loadConfig)
  val tiNic: GeneralCounter = GeneralCounter(step = U(Uic), top = io.Nic, io.loadConfig)

  val th: GeneralCounter = GeneralCounter(step = io.Toh, io.Nohw, io.loadConfig)
  val tw: GeneralCounter = GeneralCounter(step = io.Tow, io.Nohw, io.loadConfig)

  val kr: GeneralCounter = GeneralCounter(step = U(1), top = io.Krs, io.loadConfig)
  val ks: GeneralCounter = GeneralCounter(step = U(1), top = io.Krs, io.loadConfig)

  val sth: GeneralCounter = GeneralCounter(step = U(1), top = io.Toh, io.loadConfig)
  val stw: GeneralCounter = GeneralCounter(step = U(1), top = io.Tow, io.loadConfig)

  val od: GeneralCounter = GeneralCounter(step = U(1), top = io.Nd, io.loadConfig)

  when(io.PEDone) {
    od.inc()
  }

  // stw inc and clear
  when(od.willOverFlow) {
    stw.inc()
  }
  when(bigger(stw, tw, io.Nohw)) {
    stw.clear()
  }
  // sth inc and clear
  when(stw.willOverFlow | bigger(stw, tw, io.Nohw)) {
    sth.inc()
  }
  when(bigger(sth, th, io.Nohw)) {
    sth.clear()
  }

  val outputTileCalcDone: Bool = sth.willOverFlow | bigger(sth, th, io.Nohw)
  // switch pulse
  val switch: Bool = RegInit(False) // FIXME, may be wrong
  when(outputTileCalcDone) {
    switch.set()
  }
  when(switch) {
    switch.clear()
  }

  // ks and kr
  when(sth.willOverFlow | bigger(sth, th, io.Nohw)) {
    ks.inc()
  }

  when(ks.willOverFlow) {
    kr.inc()
  }

  // tw and th
  when(kr.willOverFlow) {
    tw.inc()
  }

  when(tw.willOverFlow) {
    th.inc()
  }
  // ti and to
  when(th.willOverFlow) {
    ti.inc()
    tiNic.inc()
  }

  when(ti.willOverFlow) {
    to.inc()
    toNc.inc()
  }

  // weightLoadedNum
  val weightLoadedNum: UInt = RegNextWhen(Mux(toNc.valueNext + Uc >= io.Nc, io.Nc - toNc.valueNext, U(Uc)), to.willInc | io.loadConfig.d(1))
  def bigger(a: GeneralCounter, b: GeneralCounter, u: UInt): Bool = a.value + b.value + 1 >= u && a.willInc

}
