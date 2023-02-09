package R2Plus1D

import spinal.core._
import spinal.lib._
import scala.math.sqrt
import Chainsaw._

import scala.language.postfixOps

case class LoopCtrl2D(uic: Int = Parameter.Uic, uc: Int = Parameter.Uc) extends Component {
  val width = 16
  val io = new Bundle {

    val configParaPorts: ConfigParaPorts = slave(new ConfigParaPorts(width))

    val weightRdy, fMapRdy, loadConfig: Bool = in Bool ()
    val readDone:                       Bool = in Bool ()
    val filled:                         Bool = in Bool ()
    val weightAddrBase:                 UInt = out UInt (log2Up(Parameter.weightBuffer2DDepth) bits)
    val weightLoadedNum:                UInt = out UInt (log2Up(uc) bits) setAsReg () init (0)
    val tileDone:                       Bool = out Bool ()

    val ifMapAddr:    SInt = out SInt (log2Up(Parameter.featureMapDepth) + 1 bits)
    val ifMapAddrVld: Bool = out Bool ()
  }

  import io.configParaPorts._

  val to:   GeneralCounter = GeneralCounter(step = U(1), top = NcDUcCeil, en = io.loadConfig)
  val toNc: GeneralCounter = GeneralCounter(step = U(uc), top = Nc, en = io.loadConfig)

  val ti:    GeneralCounter = GeneralCounter(step = U(1), top = NicDUicCeil, en = io.loadConfig)
  val tiNic: GeneralCounter = GeneralCounter(step = U(uic), top = Nic, en = io.loadConfig)

  val th: GeneralCounter = GeneralCounter(step = Toh, Nohw, en = io.loadConfig)
  val tw: GeneralCounter = GeneralCounter(step = Tow, Nohw, en = io.loadConfig)

  val kr: GeneralCounter = GeneralCounter(step = U(1), top = Krs, en = io.loadConfig)
  val ks: GeneralCounter = GeneralCounter(step = U(1), top = Krs, en = io.loadConfig)

  val sth: GeneralCounter = GeneralCounter(step = U(1), top = Toh, en = io.loadConfig)
  val stw: GeneralCounter = GeneralCounter(step = U(1), top = Tow, en = io.loadConfig)

  val od: GeneralCounter = GeneralCounter(step = U(1), top = Nd, en = io.loadConfig)

  when(io.filled) {
    od.inc()
  }
  // stw inc
  when(od.willOverFlow) {
    stw.inc()
  }

  // sth inc and clear
  when(stw.willOverFlow) {
    sth.inc()
  }
  // ks and kr
  when(sth.willOverFlow) {
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

  io.tileDone := sth.willOverFlow

  // -------------------weight address accumulation---------------------------------------------------------------------
  val ksWeightAddr = GeneralCounter(step = U(uc), top = Krs * uc, en = io.loadConfig)
  when(io.readDone) {
    ksWeightAddr.inc()
  }

  val krWeightAddr = GeneralCounter(step = Krs * uc, top = kernelSize * uc, en = io.loadConfig)
  when(ksWeightAddr.willOverFlow) {
    krWeightAddr.inc()
  }
  val tiWeightAddr = GeneralCounter(step = kernelSize * uc, top = NicDUicCeil * kernelSize * uc, en = io.loadConfig)
  when(th.willOverFlowIfInc & tw.willOverFlowIfInc & krWeightAddr.willOverFlow) {
    tiWeightAddr.inc()
  }

  val toWeightAddr = GeneralCounter(step = NicDUicCeil * kernelSize * uc, top = NcDUcCeil * NicDUicCeil * kernelSize * uc, en = io.loadConfig)
  when(tiWeightAddr.willOverFlow) {
    toWeightAddr.inc()
  }
  io.weightAddrBase := (ksWeightAddr.value + krWeightAddr.value + tiWeightAddr.value + toWeightAddr.value).resized
  // weightLoadedNum
  when(io.loadConfig) {
    when(Nc >= uc) {
      io.weightLoadedNum := U(uc)
    } otherwise {
      io.weightLoadedNum := (Nc % uc).resized
    }
  } elsewhen (Nc >= uc & toNc.value + (uc * 2) >= Nc & ti.willOverFlowIfInc & th.willOverFlowIfInc & tw.willOverFlowIfInc & krWeightAddr.willOverFlow & ~toNc.willOverFlowIfInc) {
    io.weightLoadedNum := (Nc % uc).resized
  }

  // -------------------------feature map address calculation-----------------------------------------------------------
  val iw: SInt = ifMapCoordinate(t = tw, i = stw, k = ks)
  val ih: SInt = ifMapCoordinate(t = th, i = sth, k = kr)
  val oh = sth.value + th.value
  val ow = stw.value + tw.value
  io.ifMapAddrVld := iw >= 0 & iw < Nihw.asSInt &
    ih >= 0 & ih < Nihw.asSInt & th.value + sth.value < Nohw & tw.value + stw.value < Nohw

  val tiIfMapAddr = GeneralCounter(step = ifMapSize, top = NicDUicCeil * ifMapSize, en = io.loadConfig)
  val thIfMapAddr = GeneralCounter(
    step = Mux(stride, (Nihw * Nd) << 1, Nihw * Nd),
    top  = Mux(stride, (Nihw * Nd * NohwDTohCei) << 1, Nihw * Nd),
    en   = io.loadConfig
  )
  val twIfMapAddr = GeneralCounter(
    step = Mux(stride, Nd << 1, Nd),
    top  = Mux(stride, (Nd * NohwDTowCei) << 1, Nd * NohwDTowCei),
    en   = io.loadConfig
  )

  val krIfMapAddr = GeneralCounter(step = Nihw * Nd, top = Nihw * Nd * Krs, en = io.loadConfig)
  val ksIfMapAddr = GeneralCounter(step = Nd, top = Nd * Krs, en = io.loadConfig)
  val sthIfMapAddr = GeneralCounter(
    step = Mux(stride, (Nihw * Nd) << 1, Nihw * Nd),
    top  = Mux(stride, (Nihw * Nd * Toh) << 1, Nihw * Nd * Toh),
    en   = io.loadConfig
  )
  val stwIfMapAddr = GeneralCounter(step = Mux(stride, Nd << 1, Nd), top = Mux(stride, (Nd * Tow) << 1, Nd * Tow), en = io.loadConfig)

  when(od.willOverFlow) {
    stwIfMapAddr.inc()
  }
  when(stw.willOverFlow) {
    sthIfMapAddr.inc()
  }
  when(sth.willOverFlow) {
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
    sthIfMapAddr.value + stwIfMapAddr.value + od.value - (Nihw + 1) * Nd * padding).resize(log2Up(Parameter.featureMapDepth) + 1).asSInt
  // -------------------------------------------------------------------------------------------------------------------

  def ifMapCoordinate(t: GeneralCounter, i: GeneralCounter, k: GeneralCounter): SInt =
    (Mux(stride, (t.value + i.value) << 1, t.value + i.value) + k.value).asSInt - padding.asSInt
}
