package R2Plus1D

import spinal.core._
import spinal.lib._
import scala.language.postfixOps
import Chainsaw._

case class LoopCtrl1D(
    uc:              Int = Parameter.Uc,
    uoc:             Int = Parameter.Uoc,
    readLatencyURAM: Int = 4,
    readLatencyBRAM: Int = 2,
    PELatency:       Int
) extends Component {
  val io = new Bundle {
    val loadConfig:       Bool              = in Bool ()
    val config:           ConfigParaPorts1D = in(ConfigParaPorts1D())
    val shortCut:         Bool              = in Bool ()
    val toWriteDOne:      Bool              = in Bool ()
    val ofMapBuffer2DRdy: Bool              = in Bool ()
    val weightAddrBase:   UInt              = out UInt (log2Up(Parameter.weightBuffer1DDepth) bits)
    val weightLoadedNum:  UInt              = out(UInt(log2Up(uoc + 1) bits)) setAsReg () init 0
    val readDone:         Bool              = in Bool ()
    val weightFilled:     Bool              = in Bool ()
    val layerReadDone:    Bool              = out Bool () setAsReg () init False
    val conv1DDone:       Bool              = out Bool () setAsReg () init (False)
    val ifMapRdEn:        Bool              = out Bool ()
    val ifMapAddr:        UInt              = out UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val ifMapAddrVld:     Bool              = out Bool ()

    val ofMapAddr:    UInt = out UInt (log2Up(Parameter.featureMapDepth) bits)
    val ofMapWriteEn: Bool = out Bool ()
    val writeAcc:     Bool = out Bool ()
    val doutEn:       Bool = out Bool ()
    val accAddr:      UInt = out UInt (log2Up(Parameter.ofMapMaxOwOhSize1D) bits)

    val willOverflowIfInc: Bool = out Bool ()

  }

  val ofMapLatency: Int = readLatencyURAM + PELatency + readLatencyBRAM

  val ow: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nohw, en = io.loadConfig)
  val oh: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nohw, en = io.loadConfig)
  val kt: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Kt, en = io.loadConfig)
  val ti: GeneralCounter = GeneralCounter(step = U(1), top = io.config.NcDUcCeil, en = io.loadConfig)
  val tiNc: GeneralCounter = GeneralCounter(
    step = U(uc),
    top  = io.config.Nc,
    en   = io.loadConfig
  )
  val od:    GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nod, en = io.loadConfig)
  val to:    GeneralCounter = GeneralCounter(step = U(1), top = io.config.NocDUocCeil, en = io.loadConfig)
  val toNoc: GeneralCounter = GeneralCounter(step = U(uoc), top = io.config.Noc, en = io.loadConfig)

  val fMapFilled: Bool = RegInit(False)

  when(io.toWriteDOne) {
    fMapFilled.set()
  } elsewhen (io.willOverflowIfInc) {
    fMapFilled.clear()
  }
  val owInc: Bool = (fMapFilled & ~io.willOverflowIfInc) | io.weightFilled

  when(owInc) {
    ow.inc()
  }

  when(ow.willOverFlow) {
    oh.inc()
  }
  when(oh.willOverFlow) {
    kt.inc()
  }
  when(kt.willOverFlow) {
    tiNc.inc()
    ti.inc()
  }
  when(ti.willOverFlow) {
    od.inc()
  }
  when(od.willOverFlow) {
    to.inc()
    toNoc.inc()
  }

  // ----------------------weight addr base accumulation ----------------------------------------------------------------
  val ktWeightAddr: GeneralCounter = GeneralCounter(step = U(uoc), top = io.config.Kt * uoc, en = io.loadConfig)
  val tiWeightAddr: GeneralCounter = GeneralCounter(
    step = io.config.Kt * uoc,
    top  = io.config.NcDUcCeil * io.config.Kt * uoc,
    en   = io.loadConfig
  )
  val toWeightAddr: GeneralCounter = GeneralCounter(
    step = io.config.NcDUcCeil * io.config.Kt * uoc,
    top  = io.config.NcDUcCeil * io.config.NocDUocCeil * io.config.Kt * uoc,
    en   = io.loadConfig
  )
  when(io.readDone) {
    ktWeightAddr.inc()
  }
  when(ktWeightAddr.willOverFlow) {
    tiWeightAddr.inc()
  }
  when(tiWeightAddr.willOverFlow & od.willOverFlowIfInc) {
    toWeightAddr.inc()
  }
  io.weightAddrBase := (ktWeightAddr.value + tiWeightAddr.value + toWeightAddr.value).resized
  when(io.loadConfig) {
    when(io.config.Noc >= uoc) {
      io.weightLoadedNum := U(uoc)
    } otherwise {
      io.weightLoadedNum := (io.config.Noc % U(uoc)).resized
    }
  } elsewhen (io.config.Noc >= U(uoc) & toNoc.value + (uoc * 2) >= io.config.Noc & ti.willOverFlowIfInc & od.willOverFlowIfInc & ktWeightAddr.willOverFlow) {
    io.weightLoadedNum := (io.config.Noc % U(uoc)).resized
  }
  io.layerReadDone.setWhen(toWeightAddr.willOverFlow)
  io.layerReadDone.clearWhen(io.loadConfig)
  // ---------------------- if Map addr accumulation -------------------------------------------------------------------

  val owIfMapAddr: GeneralCounter =
    GeneralCounter(
      step = Mux(io.shortCut, io.config.Nid << 1, io.config.Nid),
      top  = Mux(io.shortCut, io.config.Nid << 1, io.config.Nid) * io.config.Nohw,
      en   = io.loadConfig
    )
  val ohIfMapAddr: GeneralCounter = GeneralCounter(
    step = io.config.Nihw * Mux(io.shortCut, io.config.Nid << 1, io.config.Nid),
    top  = io.config.Nihw * Mux(io.shortCut, io.config.Nid << 1, io.config.Nid) * io.config.Nohw,
    en   = io.loadConfig
  )

  val tiIfMapAddr: GeneralCounter = GeneralCounter(
    step = io.config.ifMapSize,
    top  = io.config.NcDUcCeil * io.config.ifMapSize,
    en   = io.loadConfig
  )
  val odIfMapAddr: GeneralCounter = GeneralCounter(
    step = Mux(io.shortCut, U(2), Mux(io.config.stride, U(2), U(1))),
    top  = Mux(io.shortCut, io.config.Nod << 1, Mux(io.config.stride, io.config.Nod << 1, io.config.Nod)),
    en   = io.loadConfig
  )

  when(owInc) {
    owIfMapAddr.inc()
  }
  when(owIfMapAddr.willOverFlow) {
    ohIfMapAddr.inc()
  }
  when(ohIfMapAddr.willOverFlow & kt.willOverFlow) {
    tiIfMapAddr.inc()
  }
  when(tiIfMapAddr.willOverFlow) {
    odIfMapAddr.inc()
  }
  val id: SInt = (Mux(io.config.stride, od.value << 1, od.value) + kt.value - io.config.padding).asSInt
  io.ifMapAddrVld := id >= 0 & id < io.config.Nid.asSInt
  io.ifMapRdEn    := Mux(io.shortCut, io.weightFilled, io.weightFilled | fMapFilled)
  io.ifMapAddr    := (Function.CounterSum(odIfMapAddr, tiIfMapAddr, ohIfMapAddr, owIfMapAddr, kt) - Mux(io.shortCut, U(0), U(1))).resized

  // ---------------------- of Map addr accumulation -------------------------------------------------------------------
  val owOfMapAddr: GeneralCounter = GeneralCounter(step = io.config.Nod, top = io.config.Nohw * io.config.Nod, en = io.loadConfig)
  val ohOfMapAddr: GeneralCounter = GeneralCounter(
    step = io.config.Nohw * io.config.Nod,
    top  = io.config.Nohw * io.config.Nohw * io.config.Nod,
    en   = io.loadConfig
  )
  val odOfMapAddr: GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nod, en = io.loadConfig)
  val toOfMapAddr: GeneralCounter = GeneralCounter(step = io.config.ofMapSize, top = io.config.NocDUocCeil * io.config.ofMapSize, en = io.loadConfig)
  when(owInc.d(ofMapLatency)) {
    owOfMapAddr.inc()
  }
  when(owOfMapAddr.willOverFlow) {
    ohOfMapAddr.inc()
  }
  when(ohOfMapAddr.willOverFlow & (kt.willOverFlowIfInc & ti.willOverFlowIfInc).d(ofMapLatency)) {
    odOfMapAddr.inc()
  }
  when(odOfMapAddr.willOverFlow) {
    toOfMapAddr.inc()
  }
  io.ofMapWriteEn := ti.willOverFlowIfInc.d(ofMapLatency)
  io.ofMapAddr    := Function.CounterSum(owOfMapAddr, ohOfMapAddr, odOfMapAddr, toOfMapAddr).resized
  io.writeAcc     := io.weightFilled.d(readLatencyURAM + PELatency)
  io.doutEn       := ti.willOverFlowIfInc.d(readLatencyURAM + PELatency)
  io.accAddr      := (ow.value + oh.value * io.config.Nohw).d(readLatencyURAM + PELatency).resized

  io.conv1DDone.setWhen(io.readDone & io.layerReadDone)
  io.conv1DDone.clearWhen(io.loadConfig)
  io.willOverflowIfInc := ow.willOverFlowIfInc & oh.willOverFlowIfInc & kt.willOverFlowIfInc
}
