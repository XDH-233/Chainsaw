package R2Plus1D

import spinal.core._
import spinal.lib._
import scala.math.sqrt
import Chainsaw._

import scala.language.postfixOps

case class LoopCtrl2D(uic: Int = Parameter.Uic, uc: Int = Parameter.Uc, readLatency: Int = 4, PELatency: Int = 5) extends Component {
  val width = 16
  val io = new Bundle {

    val config: ConfigParaPorts2D = slave(new ConfigParaPorts2D(width))

    val weightRdy, fMapRdy, loadConfig: Bool = in Bool ()
    val readDone:                       Bool = in Bool ()
    val filled:                         Bool = in Bool ()
    val weightAddrBase:                 UInt = out UInt (log2Up(Parameter.weightBuffer2DDepth) bits)
    val weightLoadedNum:                UInt = out UInt (log2Up(uc + 1) bits) setAsReg () init 0
    val layerDone:                      Bool = out Bool () setAsReg () init False

    val ifMapAddr:    SInt = out SInt (log2Up(Parameter.featureMapDepth) + 1 bits)
    val ifMapAddrVld: Bool = out Bool ()
    val ifMapRdEn:    Bool = out Bool ()
  }

  import io.config._

  val to:   GeneralCounter = GeneralCounter(step = U(1), top = NcDUcCeil, en = io.loadConfig)
  val toNc: GeneralCounter = GeneralCounter(step = U(uc), top = Nc, en = io.loadConfig)

  val th: GeneralCounter = GeneralCounter(step = Toh, Nohw, en = io.loadConfig)
  val tw: GeneralCounter = GeneralCounter(step = Tow, Nohw, en = io.loadConfig)

  val ti:    GeneralCounter = GeneralCounter(step = U(1), top = NicDUicCeil, en = io.loadConfig)
  val tiNic: GeneralCounter = GeneralCounter(step = U(uic), top = Nic, en = io.loadConfig)

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
  // ti
  when(kr.willOverFlow) {
    ti.inc()
    tiNic.inc()
  }
  // tw and th
  when(ti.willOverFlow) {
    tw.inc()
  }
  when(tw.willOverFlow) {
    th.inc()
  }
  when(th.willOverFlow) {
    to.inc()
    toNc.inc()
  }

  // -------------------weight address accumulation---------------------------------------------------------------------
  val ksWeightAddr: GeneralCounter = GeneralCounter(step = U(uc), top = Krs * uc, en = io.loadConfig)
  when(io.readDone) {
    ksWeightAddr.inc()
  }

  val krWeightAddr: GeneralCounter = GeneralCounter(step = Krs * uc, top = kernelSize * uc, en = io.loadConfig)
  when(ksWeightAddr.willOverFlow) {
    krWeightAddr.inc()
  }
  val tiWeightAddr: GeneralCounter = GeneralCounter(step = kernelSize * uc, top = NicDUicCeil * kernelSize * uc, en = io.loadConfig)
  when(krWeightAddr.willOverFlow) {
    tiWeightAddr.inc()
  }

  val toWeightAddr: GeneralCounter = GeneralCounter(step = NicDUicCeil * kernelSize * uc, top = NcDUcCeil * NicDUicCeil * kernelSize * uc, en = io.loadConfig)
  when(tiWeightAddr.willOverFlow & th.willOverFlowIfInc & tw.willOverFlowIfInc) {
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
  val oh: UInt = sth.value + th.value
  val ow: UInt = stw.value + tw.value
  io.ifMapAddrVld := iw >= 0 & iw < Nihw.asSInt &
    ih >= 0 & ih < Nihw.asSInt & th.value + sth.value < Nohw & tw.value + stw.value < Nohw

  val thIfMapAddr: GeneralCounter = GeneralCounter(
    step = Mux(stride, (Nihw * Nd * Toh) << 1, Nihw * Nd * Toh),
    top  = Mux(stride, (Nihw * Nd * Nohw) << 1, Nihw * Nd * Nohw),
    en   = io.loadConfig
  )
  val twIfMapAddr: GeneralCounter = GeneralCounter(
    step = Mux(stride, (Nd * Tow) << 1, Nd * Tow),
    top  = Mux(stride, (Nd * Nohw) << 1, Nd * Nohw),
    en   = io.loadConfig
  )
  val tiIfMapAddr: GeneralCounter = GeneralCounter(step = ifMapSize, top = NicDUicCeil * ifMapSize, en = io.loadConfig)
  val krIfMapAddr: GeneralCounter = GeneralCounter(step = Nihw * Nd, top = Nihw * Nd * Krs, en = io.loadConfig)
  val ksIfMapAddr: GeneralCounter = GeneralCounter(step = Nd, top = Nd * Krs, en = io.loadConfig)
  val sthIfMapAddr: GeneralCounter = GeneralCounter(
    step = Mux(stride, (Nihw * Nd) << 1, Nihw * Nd),
    top  = Mux(stride, (Nihw * Nd * Toh) << 1, Nihw * Nd * Toh),
    en   = io.loadConfig
  )
  val stwIfMapAddr: GeneralCounter = GeneralCounter(step = Mux(stride, Nd << 1, Nd), top = Mux(stride, (Nd * Tow) << 1, Nd * Tow), en = io.loadConfig)

  when(od.willOverFlow) {
    stwIfMapAddr.inc()
  }
  when(stw.willOverFlow) {
    sthIfMapAddr.inc()
  }
  when(sthIfMapAddr.willOverFlow) {
    ksIfMapAddr.inc()
  }
  when(ksIfMapAddr.willOverFlow) {
    krIfMapAddr.inc()
  }
  when(krIfMapAddr.willOverFlow) {
    tiIfMapAddr.inc()
  }
  when(tiIfMapAddr.willOverFlow) {
    twIfMapAddr.inc()
  }
  when(twIfMapAddr.willOverFlow) {
    thIfMapAddr.inc()
  }

  io.ifMapAddr := (tiIfMapAddr.value + thIfMapAddr.value + twIfMapAddr.value + krIfMapAddr.value + ksIfMapAddr.value +
    sthIfMapAddr.value + stwIfMapAddr.value + od.value - (Nihw + 1) * Nd * padding).resize(log2Up(Parameter.featureMapDepth) + 1).asSInt
  io.layerDone.setWhen(toWeightAddr.willOverFlow)
  io.layerDone.clearWhen(io.loadConfig)
  io.ifMapRdEn := io.filled

  // -------------------------------of map Addr-------------------------------------------------------------------------
  val odOfMapAddr:  GeneralCounter = GeneralCounter(step = U(1), top = Nd, en = io.loadConfig)
  val stwOfMapAddr: GeneralCounter = GeneralCounter(step = Nd, top = Tow * Nd, en = io.loadConfig)
  val sthOfMapAddr: GeneralCounter = GeneralCounter(step = Nohw * Nd, top = Toh * Nohw * Nd, en = io.loadConfig)
  val twOfmapAddr:  GeneralCounter = GeneralCounter(step = Nd, top = NohwDTowCei * Nd, en = io.loadConfig)
  val thOfMapAddr:  GeneralCounter = GeneralCounter(step = Nohw * Nd, top = NohwDTohCei * Nohw * Nd, en = io.loadConfig)
  val toOfmapAddr:  GeneralCounter = GeneralCounter(step = ofMapSize, top = ofMapSize * NcDUcCeil, en = io.loadConfig)
  when(io.filled.d(readLatency + PELatency)) {
    odOfMapAddr.inc()
  }
  when(odOfMapAddr.willOverFlow) {
    stwOfMapAddr.inc()
  }
  when(stwOfMapAddr.willOverFlow) {
    sthOfMapAddr.inc()
  }
  when(sthOfMapAddr.willOverFlow & kr.willOverFlowIfInc.d(readLatency + PELatency) & ks.willOverFlowIfInc.d(readLatency + PELatency)) {
    twOfmapAddr.inc()
  }
  when(twOfmapAddr.willOverFlow) {
    thOfMapAddr.inc()
  }
  when(thOfMapAddr.willOverFlow & ti.willOverFlowIfInc.d(readLatency + PELatency)) {
    toOfmapAddr.inc()
  }
  val ohD:          UInt = (th.value + sth.value).d(readLatency + PELatency)
  val owD:          UInt = (tw.value + stw.value).d(readLatency + PELatency)
  val ofMapAddr:    UInt = (odOfMapAddr.value + stwOfMapAddr.value + sthOfMapAddr.value + twOfmapAddr.value + thOfMapAddr.value + toOfmapAddr.value)
  val ofMapAddrVld: Bool = (th.value + sth.value < Nohw & tw.value + stw.value < Nohw).d(readLatency + PELatency)
  val ofMapWriteEn: Bool = io.filled.d(readLatency + PELatency) & ofMapAddrVld & ti.willOverFlowIfInc.d(readLatency + PELatency)
  val accRamAdrr:   UInt = odOfMapAddr.value + stwOfMapAddr.value + sthOfMapAddr.value
  def ifMapCoordinate(t: GeneralCounter, i: GeneralCounter, k: GeneralCounter): SInt =
    (Mux(stride, (t.value + i.value) << 1, t.value + i.value) + k.value).asSInt - padding.asSInt
}
