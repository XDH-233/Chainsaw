package R2Plus1D

import spinal.core._
import spinal.lib._
import scala.math.sqrt
import Chainsaw._

import scala.language.postfixOps

case class LoopCtrl2D(uic: Int = Parameter.Uic, uc: Int = Parameter.Uc, readLatencyURAM: Int = 5, PELatency: Int = 5, accLatency: Int = 2) extends Component {
  val io = new Bundle {

    val config: ConfigParaPorts2D = in(ConfigParaPorts2D())

    val loadConfig:      Bool = in Bool ()
    val readDone:        Bool = in Bool ()
    val filled:          Bool = in Bool ()
    val weightAddrBase:  UInt = out UInt (log2Up(Parameter.weightBuffer2DDepth) bits)
    val weightLoadedNum: UInt = out UInt (log2Up(uc + 1) bits) setAsReg () init 0
    val weightSwitch:    Bool = out Bool ()

    val layerDone:   Bool = out Bool () setAsReg () init False
    val toWriteDone: Bool = out Bool () setAsReg () init False

    val ifMapAddr:    SInt = out SInt (log2Up(Parameter.featureMapDepth) + 1 bits)
    val ifMapAddrVld: Bool = out Bool ()
    val ifMapRdEn:    Bool = out Bool ()

    val accRamAddr: UInt = out UInt (log2Up(uc) bits)
    val writeAcc:   Bool = out Bool ()
    val doutEn:     Bool = out Bool ()

    val ofMapAddr:        UInt = out UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val ofMapAddrVld:     Bool = out Bool ()
    val ofMapWe:          Bool = out Bool ()
    val ofMapBuffer2DRdy: Bool = out Bool () setAsReg () init False
  }

  val loopAddr2D: LoopAddr2D = LoopAddr2D(uc = uc)
  loopAddr2D.io.loadConfig := io.loadConfig
  loopAddr2D.io.configIn.assignAllByName(io.config)
  val ifMapAddrTail: UInt =
    RegNextWhen((io.config.Nihw + 1) * io.config.Nd + Mux(io.config.padding, ((io.config.Nihw + 1) * io.config.Nd) << 1, U(0)), io.loadConfig)

  val tc:   GeneralCounter = GeneralCounter(step = U(1), top = io.config.NcDTcCeil, en = loopAddr2D.io.loadCounter)
  val tcNc: GeneralCounter = GeneralCounter(step = io.config.Tc, top = io.config.Nc, en = loopAddr2D.io.loadCounter)

  val to:   GeneralCounter = GeneralCounter(step = U(1), top = io.config.TcDUcCeil, en = loopAddr2D.io.loadCounter)
  val toNc: GeneralCounter = GeneralCounter(step = U(uc), top = io.config.Tc, en = loopAddr2D.io.loadCounter)

  val th: GeneralCounter = GeneralCounter(step = io.config.Toh, io.config.Nohw, en = loopAddr2D.io.loadCounter)
  val tw: GeneralCounter = GeneralCounter(step = io.config.Tow, io.config.Nohw, en = loopAddr2D.io.loadCounter)

  val ti:    GeneralCounter = GeneralCounter(step = U(1), top = io.config.NicDUicCeil, en = loopAddr2D.io.loadCounter)
  val tiNic: GeneralCounter = GeneralCounter(step = U(uic), top = io.config.Nic, en = loopAddr2D.io.loadCounter)

  val kr: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Krs, en = loopAddr2D.io.loadCounter)
  val ks: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Krs, en = loopAddr2D.io.loadCounter)

  val sth: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Toh, en = loopAddr2D.io.loadCounter)
  val stw: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Tow, en = loopAddr2D.io.loadCounter)

  val od: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nd, en = loopAddr2D.io.loadCounter)

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
  when(to.willOverFlow) {
    tc.inc()
    tcNc.inc()
  }

  // -------------------weight address accumulation---------------------------------------------------------------------
  val ksWeightAddr: GeneralCounter = GeneralCounter(step = U(uc), top = loopAddr2D.io.KrsTUc, en = loopAddr2D.io.loadCounter)
  when(io.readDone) {
    ksWeightAddr.inc()
  }

  val krWeightAddr: GeneralCounter = GeneralCounter(step = loopAddr2D.io.KrsTUc, top = loopAddr2D.io.kSizeTUc, en = loopAddr2D.io.loadCounter)
  when(ksWeightAddr.willOverFlow) {
    krWeightAddr.inc()
  }
  val tiWeightAddr: GeneralCounter =
    GeneralCounter(step = loopAddr2D.io.kSizeTUc, top = loopAddr2D.io.NicDUicCeiTKSizeTUc, en = loopAddr2D.io.loadCounter)
  when(krWeightAddr.willOverFlow) {
    tiWeightAddr.inc()
  }

  val toWeightAddr: GeneralCounter = GeneralCounter(
    step = loopAddr2D.io.NicDUicCeiTKSizeTUc,
    top  = loopAddr2D.io.NcDUcCeilTNicDUicCeiTKSizeTUc,
    en   = loopAddr2D.io.loadCounter
  )
  when(tiWeightAddr.willOverFlow & th.willOverFlowIfInc & tw.willOverFlowIfInc) {
    toWeightAddr.inc()
  }
  io.weightAddrBase := RegNextWhen(
    Vec(ksWeightAddr.valueNext, krWeightAddr.valueNext, tiWeightAddr.valueNext, toWeightAddr.valueNext)
      .reduceBalancedTree(_ + _)
      .resize(log2Up(Parameter.weightBuffer2DDepth)),
    ksWeightAddr.willInc
  ) init 0

  io.weightSwitch := toWeightAddr.willOverFlow

  // weightLoadedNum
  when(io.loadConfig) {
    when(io.config.Nc >= uc) {
      io.weightLoadedNum := U(uc)
    } otherwise {
      io.weightLoadedNum := (io.config.Nc % uc).resized
    }
  } elsewhen (loopAddr2D.io.configReg.Nc >= uc & toNc.value + (uc * 2) >= loopAddr2D.io.configReg.Nc & ti.willOverFlowIfInc & th.willOverFlowIfInc & tw.willOverFlowIfInc & krWeightAddr.willOverFlow & ~toNc.willOverFlowIfInc) {
    io.weightLoadedNum := (loopAddr2D.io.configReg.Nc % uc).resized
  }

  // -------------------------feature map address calculation-----------------------------------------------------------
  val iw: SInt = ifMapCoordinate(t = tw, i = stw, k = ks)
  val ih: SInt = ifMapCoordinate(t = th, i = sth, k = kr)
  val oh: UInt = sth.value + th.value
  val ow: UInt = stw.value + tw.value
  io.ifMapAddrVld := iw >= 0 & iw < loopAddr2D.io.configReg.Nihw.asSInt &
    ih >= 0 & ih < loopAddr2D.io.configReg.Nihw.asSInt & oh < loopAddr2D.io.configReg.Nohw & ow < loopAddr2D.io.configReg.Nohw

  val thIfMapAddr: GeneralCounter = GeneralCounter(
    step = loopAddr2D.io.TohTNihwTNd + Mux(io.config.stride, loopAddr2D.io.TohTNihwTNd, U(0)),
    top  = loopAddr2D.io.NohwTNihwTNd + Mux(io.config.stride, loopAddr2D.io.NohwTNihwTNd, U(0)),
    en   = loopAddr2D.io.loadCounter
  )
  val twIfMapAddr: GeneralCounter = GeneralCounter(
    step = loopAddr2D.io.NdTTow + Mux(io.config.stride, loopAddr2D.io.NdTTow, U(0)),
    top  = loopAddr2D.io.NohwTNd + Mux(io.config.stride, loopAddr2D.io.NohwTNd, U(0)),
    en   = loopAddr2D.io.loadCounter
  )
  val tiIfMapAddr: GeneralCounter =
    GeneralCounter(step = io.config.ifMapSize, top = loopAddr2D.io.ifMapSizeTNicDUicCeil, en = loopAddr2D.io.loadCounter)
  val krIfMapAddr: GeneralCounter =
    GeneralCounter(step = loopAddr2D.io.NihwTNd, top = loopAddr2D.io.NihwTNdTKrs, en = loopAddr2D.io.loadCounter)
  val ksIfMapAddr: GeneralCounter = GeneralCounter(step = io.config.Nd, top = loopAddr2D.io.NdTKrs, en = loopAddr2D.io.loadCounter)
  val sthIfMapAddr: GeneralCounter = GeneralCounter(
    step = loopAddr2D.io.NihwTNd + Mux(io.config.stride, loopAddr2D.io.NihwTNd, U(0)),
    top  = loopAddr2D.io.NihwTNdTTow + Mux(io.config.stride, loopAddr2D.io.NihwTNdTTow, U(0)),
    en   = loopAddr2D.io.loadCounter
  )
  val stwIfMapAddr: GeneralCounter = GeneralCounter(
    step = Mux(io.config.stride, io.config.Nd << 1, io.config.Nd),
    top  = loopAddr2D.io.NdTTow + Mux(io.config.stride, loopAddr2D.io.NdTTow, U(0)),
    en   = loopAddr2D.io.loadCounter
  )

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

  io.ifMapAddr := (Function.CounterSum(
    tiIfMapAddr,
    thIfMapAddr,
    twIfMapAddr,
    krIfMapAddr,
    ksIfMapAddr,
    sthIfMapAddr,
    stwIfMapAddr,
    od
  ) - ifMapAddrTail).resize(log2Up(Parameter.featureMapDepth) + 1).asSInt
  io.layerDone.setWhen(tcNc.value + toNc.valueNext > loopAddr2D.io.configReg.Nc & to.willInc)
  io.layerDone.clearWhen(io.loadConfig)
  io.ifMapRdEn := io.filled

  // -------------------------------of map Addr-------------------------------------------------------------------------
  val odOfMapAddr:  GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nd, en = loopAddr2D.io.loadCounter)
  val stwOfMapAddr: GeneralCounter = GeneralCounter(step = io.config.Nd, top = loopAddr2D.io.NdTTow, en = loopAddr2D.io.loadCounter)
  val sthOfMapAddr: GeneralCounter =
    GeneralCounter(step = loopAddr2D.io.NohwTNd, top = loopAddr2D.io.TohTNohwTNd, en = loopAddr2D.io.loadCounter)
  val twOfMapAddr: GeneralCounter = GeneralCounter(step = loopAddr2D.io.NdTTow, top = loopAddr2D.io.NohwTNd, en = loopAddr2D.io.loadCounter)
  val thOfMapAddr: GeneralCounter =
    GeneralCounter(step = loopAddr2D.io.TohTNohwTNd, top = loopAddr2D.io.configReg.ofMapSize, en = loopAddr2D.io.loadCounter)
  val toOfMapAddr: GeneralCounter = GeneralCounter(step = io.config.ofMapSize, top = loopAddr2D.io.ofMapSizeTcDUcCeil, en = loopAddr2D.io.loadCounter)
  val tcOfMapAddr: GeneralCounter =
    GeneralCounter(step = loopAddr2D.io.ofMapSizeTcDUcCeil, top = loopAddr2D.io.NcDTcofMapSizeTcDUcCeil, en = loopAddr2D.io.loadCounter)
  val ofMapAddrLatency: Int = readLatencyURAM + accLatency + PELatency - 1
  when(io.filled.d(ofMapAddrLatency)) {
    odOfMapAddr.inc()
  }
  when(odOfMapAddr.willOverFlow) {
    stwOfMapAddr.inc()
  }
  when(stwOfMapAddr.willOverFlow) {
    sthOfMapAddr.inc()
  }
  when(
    sthOfMapAddr.willOverFlow & (kr.willOverFlowIfInc & ks.willOverFlowIfInc & ti.willOverFlowIfInc).d(ofMapAddrLatency)
  ) {
    twOfMapAddr.inc()
  }
  when(twOfMapAddr.willOverFlow) {
    thOfMapAddr.inc()
  }
  when(thOfMapAddr.willOverFlow) {
    toOfMapAddr.inc()
  }
  when(toWeightAddr.willOverFlow) {
    tcOfMapAddr.inc()
  }

  io.ofMapAddr    := Function.CounterSum(odOfMapAddr, stwOfMapAddr, sthOfMapAddr, twOfMapAddr, thOfMapAddr, toOfMapAddr, tcOfMapAddr).resized
  io.ofMapAddrVld := (th.value + sth.value < loopAddr2D.io.configReg.Nohw & tw.value + stw.value < loopAddr2D.io.configReg.Nohw) d (ofMapAddrLatency)
  // -------------------------------accRAM Addr-------------------------------------------------------------------------
  val accRAMAddrCounter: GeneralCounter =
    GeneralCounter(step = U(1), top = loopAddr2D.io.configReg.Nd * loopAddr2D.io.configReg.Tow * loopAddr2D.io.configReg.Toh, en = loopAddr2D.io.loadCounter)
  io.accRamAddr := accRAMAddrCounter.value.resized
  when(io.ifMapRdEn.d(readLatencyURAM + PELatency)) {
    accRAMAddrCounter.inc()
  }
  io.writeAcc := io.ifMapRdEn.d(readLatencyURAM + PELatency)
  io.doutEn   := ti.willOverFlowIfInc.d(readLatencyURAM + PELatency)
  io.ofMapWe  := ti.willOverFlowIfInc.d(ofMapAddrLatency)
  io.ofMapBuffer2DRdy.setWhen((io.readDone & io.layerDone).d(ofMapAddrLatency))
  io.ofMapBuffer2DRdy.clearWhen(io.filled)

  io.toWriteDone := (to.willInc.d(readLatencyURAM + PELatency + accLatency))

  // --------------------------------------------------------------------------------------------------------------------
  // clear counters
  when(io.layerDone & io.readDone) {
    Seq(
      od,
      sth,
      stw,
      sth,
      ks,
      kr,
      ti,
      tiNic,
      tw,
      th,
      to,
      toNc,
      ksWeightAddr,
      krWeightAddr,
      tiWeightAddr,
      toWeightAddr,
      stwIfMapAddr,
      sthIfMapAddr,
      ksIfMapAddr,
      krIfMapAddr,
      tiIfMapAddr
    ).foreach(_.clear())
    when((io.layerDone & io.readDone).d(readLatencyURAM + PELatency + accLatency)) {
      Seq(odOfMapAddr, stwOfMapAddr, sthOfMapAddr, twOfMapAddr, thOfMapAddr, toOfMapAddr).foreach(_.clear())
    }
  }
  def ifMapCoordinate(t: GeneralCounter, i: GeneralCounter, k: GeneralCounter): SInt =
    (Mux(loopAddr2D.io.configReg.stride, (t.value + i.value) << 1, t.value + i.value) + k.value).asSInt - Mux(
      loopAddr2D.io.configReg.padding,
      U(3),
      U(1)
    ).asSInt
}
