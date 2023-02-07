package R2Plus1D

import spinal.core._
import spinal.lib._
import Parameter._
import scala.math.sqrt
import Chainsaw._

import scala.language.postfixOps

case class LoopCtrl2D(uic: Int = Uic, uc: Int = Uc) extends Component {
  val width = 16
  val io = new Bundle {
    val Nic, Nc, Nohw, Nd, Krs, Tow, Toh, Nihw: UInt = in UInt (width bits)
    val NcDUcCeil, NicDUicCeil:                 UInt = in UInt (4 bits)
    val kernelSize:                             UInt = in UInt (6 bits)
    val ifMapSize:                              UInt = in UInt (log2Up(ifMapSizeMax2D) bits)
    val weightRdy, fMapRdy, PEDone, loadConfig: Bool = in Bool ()
    val weightAddrBase:                         UInt = out UInt (log2Up(weightBuffer2DDepth) bits)
    val tileDone:                               Bool = out Bool ()
    val NohwDTohCei:                            UInt = in UInt (width bits)
    val NohwDTowCei:                            UInt = in UInt (width bits)
    val stride:                                 Bool = in Bool () // 0 -> 1, 1 -> 2
    val padding:                                UInt = in UInt (3 bits)
    val ifMapAddr:                              SInt = out SInt (log2Up(featureMapDepth) + 1 bits)
    val ifMapAddrVld:                           Bool = out Bool ()
  }

  val to:   GeneralCounter = GeneralCounter(step = U(1), top = io.NcDUcCeil, en = io.loadConfig)
  val toNc: GeneralCounter = GeneralCounter(step = U(uc), top = io.Nc, en = io.loadConfig)

  val ti:    GeneralCounter = GeneralCounter(step = U(1), top = io.NicDUicCeil, en = io.loadConfig)
  val tiNic: GeneralCounter = GeneralCounter(step = U(uic), top = io.Nic, en = io.loadConfig)

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
  val weightLoadedNum: UInt = RegNextWhen(io.Nc - toNc.value, toNc.willOverFlowIfInc) init (U(uc))
  io.tileDone := sth.willOverFlow | bigger(sth, th, io.Nohw)

  // -------------------weight address accumulation---------------------------------------------------------------------
  val ksWeightAddr = GeneralCounter(step = U(uc), top = io.Krs * uc, en = io.loadConfig)
  when(io.tileDone) {
    ksWeightAddr.inc()
  }

  val krWeightAddr = GeneralCounter(step = io.Krs * uc, top = io.kernelSize * uc, en = io.loadConfig)
  when(ks.willOverFlow) {
    krWeightAddr.inc()
  }
  val tiWeightAddr = GeneralCounter(step = io.kernelSize * uc, top = io.NicDUicCeil * io.kernelSize * uc, en = io.loadConfig)
  when(th.willOverFlow) {
    tiWeightAddr.inc()
  }

  val toWeightAddr = GeneralCounter(step = io.NicDUicCeil * io.kernelSize * uc, top = io.NcDUcCeil * io.NicDUicCeil * io.kernelSize * uc, en = io.loadConfig)
  when(ti.willOverFlow) {
    toWeightAddr.inc()
  }
  io.weightAddrBase := (ksWeightAddr.value + krWeightAddr.value + tiWeightAddr.value + toWeightAddr.value).resized

  // -------------------------feature map address calculation-----------------------------------------------------------
  val iw: SInt = ifMapCoordinate(t = tw, i = stw, k = ks)
  val ih: SInt = ifMapCoordinate(t = th, i = sth, k = kr)
  io.ifMapAddrVld := ~(iw < 0 | iw >= io.Nihw.asSInt | ih < 0 | ih >= io.Nihw.asSInt)

  val tiIfMapAddr = GeneralCounter(step = io.ifMapSize, top = io.NicDUicCeil * io.ifMapSize, en = io.loadConfig)
  val thIfMapAddr = GeneralCounter(
    step = Mux(io.stride, (io.Nihw * io.Nd) << 1, io.Nihw * io.Nd),
    top  = Mux(io.stride, (io.Nihw * io.Nd * io.NohwDTohCei) << 1, io.Nihw * io.Nd),
    en   = io.loadConfig
  )
  val twIfMapAddr = GeneralCounter(
    step = Mux(io.stride, io.Nd << 1, io.Nd),
    top  = Mux(io.stride, (io.Nd * io.NohwDTowCei) << 1, io.Nd * io.NohwDTowCei),
    en   = io.loadConfig
  )

  val krIfMapAddr = GeneralCounter(step = io.Nihw * io.Nd, top = io.Nihw * io.Nd * io.Krs, en = io.loadConfig)
  val ksIfMapAddr = GeneralCounter(step = io.Nd, top = io.Nd * io.Krs, en = io.loadConfig)
  val sthIfMapAddr = GeneralCounter(
    step = Mux(io.stride, (io.Nihw * io.Nd) << 1, io.Nihw * io.Nd),
    top  = Mux(io.stride, (io.Nihw * io.Nd * io.Toh) << 1, io.Nihw * io.Nd * io.Toh),
    en   = io.loadConfig
  )
  val stwIfMapAddr = GeneralCounter(step = Mux(io.stride, io.Nd << 1, io.Nd), top = Mux(io.stride, (io.Nd * io.Tow) << 1, io.Nd * io.Tow), en = io.loadConfig)

  when(od.willOverFlow) {
    stwIfMapAddr.inc()
  }
  when(stw.willOverFlow | bigger(stw, tw, io.Nohw)) {
    sthIfMapAddr.inc()
  }
  when(sth.willOverFlow | bigger(sth, th, io.Nohw)) {
    ksIfMapAddr.inc()
  }
  when(ks.willOverFlow) {
    krIfMapAddr.inc()
  }
  when(kr.willOverFlow) {
    twIfMapAddr.inc()
  }
  when(tw.willOverFlow) {
    thIfMapAddr.inc()
  }
  when(th.willOverFlow) {
    tiIfMapAddr.inc()
  }

  io.ifMapAddr := (tiIfMapAddr.value + thIfMapAddr.value + twIfMapAddr.value + krIfMapAddr.value + ksIfMapAddr.value +
    sthIfMapAddr.value + stwIfMapAddr.value + od.value - (io.Nihw + 1) * io.Nd * io.padding).resize(log2Up(featureMapDepth) + 1).asSInt
  // -------------------------------------------------------------------------------------------------------------------
  def bigger(a: GeneralCounter, b: GeneralCounter, u: UInt): Bool = a.value + b.value + 1 >= u && a.willInc

  def ifMapCoordinate(t: GeneralCounter, i: GeneralCounter, k: GeneralCounter): SInt =
    (Mux(io.stride, (t.value + i.value) << 1, t.value + i.value) + k.value).asSInt - io.padding.asSInt
}
