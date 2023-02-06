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
    val weightAddrBase                         = out UInt (log2Up(weightBuffer2DDepth) bits)
    val tileDone                               = out Bool ()
  }

  val to:   GeneralCounter = GeneralCounter(step = U(1), top = io.NcDUcCeil, en = io.loadConfig)
  val toNc: GeneralCounter = GeneralCounter(step = U(Uc), top = io.Nc, en = io.loadConfig)

  val ti:    GeneralCounter = GeneralCounter(step = U(1), top = io.NicDUicCeil, en = io.loadConfig)
  val tiNic: GeneralCounter = GeneralCounter(step = U(Uic), top = io.Nic, en = io.loadConfig)

  val th: GeneralCounter = GeneralCounter(step = io.Toh, io.Nohw, en = io.loadConfig)
  val tw: GeneralCounter = GeneralCounter(step = io.Tow, io.Nohw, en = io.loadConfig)

  val kr: GeneralCounter = GeneralCounter(step = U(1), top = io.Krs, en = io.loadConfig)
  val ks: GeneralCounter = GeneralCounter(step = U(1), top = io.Krs, en = io.loadConfig)

  val sth: GeneralCounter = GeneralCounter(step = U(1), top = io.Toh, en = io.loadConfig)
  val stw: GeneralCounter = GeneralCounter(step = U(1), top = io.Tow, en = io.loadConfig)

  val od: GeneralCounter = GeneralCounter(step = U(1), top = io.Nd, en = io.loadConfig)

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
  val weightLoadedNum: UInt = RegNextWhen(io.Nc - toNc.value, toNc.willOverFlowIfInc) init (U(Uc))
  io.tileDone := sth.willOverFlow | bigger(sth, th, io.Nohw)

  // -------------------weight address accumulation---------------------------------------------------------------------
  val ksWeightAddr = GeneralCounter(step = U(Uc), top = io.Krs * Uc, en = io.loadConfig)
  when(io.tileDone) {
    ksWeightAddr.inc()
  }

  val krWeightAddr = GeneralCounter(step = io.Krs * Uc, top = io.kernelSize * Uc, en = io.loadConfig)
  when(ks.willOverFlow) {
    krWeightAddr.inc()
  }
  val tiWeightAddr = GeneralCounter(step = io.kernelSize * Uc, top = io.NicDUicCeil * io.kernelSize * Uc, en = io.loadConfig)
  when(th.willOverFlow) {
    tiWeightAddr.inc()
  }

  val toWeightAddr = GeneralCounter(step = io.NicDUicCeil * io.kernelSize * Uc, top = io.NcDUcCeil * io.NicDUicCeil * io.kernelSize * Uc, en = io.loadConfig)
  when(ti.willOverFlow) {
    toWeightAddr.inc()
  }
  io.weightAddrBase := (ksWeightAddr.value + krWeightAddr.value + tiWeightAddr.value + toWeightAddr.value).resized

  // -------------------------feature map address calculation-----------------------------------------------------------

  def bigger(a: GeneralCounter, b: GeneralCounter, u: UInt): Bool = a.value + b.value + 1 >= u && a.willInc

}
