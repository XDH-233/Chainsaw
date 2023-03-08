package R2Plus1D

import spinal.core._
import spinal.lib._
import scala.language.postfixOps
import Chainsaw._

case class LoopCtrl1D(
    uoc:             Int = Parameter.Uoc,
    readLatencyURAM: Int = 5,
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

  val fMapReadLatency: Int  = readLatencyURAM + 1
  val shortCutReg:     Bool = io.shortCut.d(4)

  val loopAddr1D: LoopAddr1D = LoopAddr1D(uoc = uoc)
  loopAddr1D.io.configIn.assignAllByName((io.config))
  loopAddr1D.io.loadConfig := io.loadConfig

  val ofMapLatency: Int = fMapReadLatency + PELatency + readLatencyBRAM

  val ow:     GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nohw, en = io.loadConfig)
  val oh:     GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nohw, en = io.loadConfig)
  val kt:     GeneralCounter = GeneralCounter(step = U(1), top = io.config.Kt, en = io.loadConfig)
  val ti:     GeneralCounter = GeneralCounter(step = U(1), top = io.config.TocDUocCeil, en = io.loadConfig)
  val tiNc:   GeneralCounter = GeneralCounter(step = io.config.Uc, top = io.config.Nc, en = io.loadConfig)
  val od:     GeneralCounter = GeneralCounter(step = U(1), top = io.config.Nod, en = io.loadConfig)
  val to:     GeneralCounter = GeneralCounter(step = U(1), top = io.config.TocDUocCeil, en = io.loadConfig)
  val toNoc:  GeneralCounter = GeneralCounter(step = U(uoc), top = io.config.Toc, en = io.loadConfig)
  val toc:    GeneralCounter = GeneralCounter(step = U(1), top = io.config.NocDTcCeil, io.loadConfig)
  val tocNoc: GeneralCounter = GeneralCounter(step = io.config.Toc, top = io.config.Noc, io.loadConfig)

  val fMapFilled: Bool = RegInit(False)

  when(io.toWriteDOne) {
    fMapFilled.set()
  } elsewhen (io.willOverflowIfInc) {
    fMapFilled.clear()
  }
  val owInc: Bool = fMapFilled | io.weightFilled

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

  when(to.willOverFlow) {
    toc.inc()
    tocNoc.inc()
  }

  // ----------------------weight addr base accumulation ----------------------------------------------------------------
  val ktWeightAddr: GeneralCounter = GeneralCounter(step = U(uoc), top = loopAddr1D.io.ktTUoc, en = loopAddr1D.io.loadCounter)
  val tiWeightAddr: GeneralCounter = GeneralCounter(
    step = loopAddr1D.io.ktTUoc,
    top  = loopAddr1D.io.NcDUcCeilTKtUoc,
    en   = loopAddr1D.io.loadCounter
  )
  val toWeightAddr: GeneralCounter = GeneralCounter(
    step = loopAddr1D.io.NcDUcCeilTKtUoc,
    top  = loopAddr1D.io.TocDUocCeilTNcDUcCeilTKtUoc,
    en   = loopAddr1D.io.loadCounter
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
  io.weightAddrBase := Function.CounterSum(ktWeightAddr, tiWeightAddr, toWeightAddr).resized
  when(io.loadConfig) {
    when(io.config.Noc >= uoc) {
      io.weightLoadedNum := U(uoc)
    } otherwise {
      io.weightLoadedNum := (io.config.Noc % U(uoc)).resized // FIXME
    }
  } elsewhen (loopAddr1D.io.configReg.Noc >= U(
    uoc
  ) & toNoc.value + (uoc * 2) >= loopAddr1D.io.configReg.Noc & ti.willOverFlowIfInc & od.willOverFlowIfInc & ktWeightAddr.willOverFlow) {
    io.weightLoadedNum := (loopAddr1D.io.configReg.Noc % U(uoc)).resized // FIXME %
  }
  io.layerReadDone.setWhen(toNoc.valueNext + tocNoc.value >= loopAddr1D.io.NocDUocCeilTUoc & toNoc.willInc)
  io.layerReadDone.clearWhen(io.loadConfig)
  // ---------------------- if Map addr accumulation -------------------------------------------------------------------

  val owIfMapAddr: GeneralCounter =
    GeneralCounter(
      step = Mux(shortCutReg, loopAddr1D.io.configReg.Nid << 1, loopAddr1D.io.configReg.Nid),
      top  = loopAddr1D.io.NohwTNid + Mux(shortCutReg, loopAddr1D.io.NohwTNid, U(0)),
      en   = loopAddr1D.io.loadCounter
    )
  val ohIfMapAddr: GeneralCounter = GeneralCounter(
    step = loopAddr1D.io.NihwTNid + Mux(shortCutReg, loopAddr1D.io.NihwTNid, U(0)),
    top  = loopAddr1D.io.NohwTNihwTNid + Mux(shortCutReg, loopAddr1D.io.NohwTNihwTNid, U(0)),
    en   = loopAddr1D.io.loadCounter
  )

  val tiIfMapAddr: GeneralCounter = GeneralCounter(
    step = io.config.ifMapSize,
    top  = loopAddr1D.io.ifMapSizeTNicDUicCeil,
    en   = loopAddr1D.io.loadCounter
  )

  val odIfMapAddr: GeneralCounter = GeneralCounter(
    step = Mux(shortCutReg | loopAddr1D.io.configReg.stride, U(2), U(1)),
    top  = loopAddr1D.io.configReg.Nod + Mux(shortCutReg | loopAddr1D.io.configReg.stride, loopAddr1D.io.configReg.Nod, U(0)),
    en   = loopAddr1D.io.loadCounter
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
  val id: SInt = (Mux(loopAddr1D.io.configReg.stride, od.value << 1, od.value) + kt.value - loopAddr1D.io.configReg.padding).asSInt
  io.ifMapAddrVld := (id >= 0 & id < loopAddr1D.io.configReg.Nid.asSInt).d(1)
  io.ifMapRdEn    := (Mux(shortCutReg, io.weightFilled, io.weightFilled | fMapFilled)).d(1)
  io.ifMapAddr := (Function.CounterSum(odIfMapAddr, tiIfMapAddr, ohIfMapAddr, owIfMapAddr, kt) - Mux(shortCutReg, U(0), U(1))).d(1).resized // pipe for timing

  // ---------------------- of Map addr accumulation -------------------------------------------------------------------
  val owOfMapAddr: GeneralCounter = GeneralCounter(step = io.config.Nod, top = loopAddr1D.io.NohwTNod, en = loopAddr1D.io.loadCounter)
  val ohOfMapAddr: GeneralCounter = GeneralCounter(step = loopAddr1D.io.NohwTNod, top = loopAddr1D.io.configReg.ofMapSize, en = loopAddr1D.io.loadCounter)
  val odOfMapAddr: GeneralCounter = GeneralCounter(step = U(1), top = loopAddr1D.io.configReg.Nod, en = loopAddr1D.io.loadCounter)
  val toOfMapAddr: GeneralCounter =
    GeneralCounter(step = io.config.ofMapSize, top = loopAddr1D.io.ofMapSizeTTocDUocCeil, en = loopAddr1D.io.loadCounter)
  val tocOfMapAddr: GeneralCounter =
    GeneralCounter(step = loopAddr1D.io.ofMapSizeTTocDUocCeil, top = loopAddr1D.io.NocDTocCeilTofMapSizeTTocDUocCeil, loopAddr1D.io.loadCounter)

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
  when(toOfMapAddr.willOverFlow) {
    tocOfMapAddr.inc()
  }
  io.ofMapWriteEn := ti.willOverFlowIfInc.d(ofMapLatency)
  io.ofMapAddr    := Function.CounterSum(owOfMapAddr, ohOfMapAddr, odOfMapAddr, toOfMapAddr).resized
  io.writeAcc     := io.weightFilled.d(fMapReadLatency + PELatency)
  io.doutEn       := ti.willOverFlowIfInc.d(fMapReadLatency + PELatency)
  io.accAddr      := (ow.value + oh.value * loopAddr1D.io.configReg.Nohw).d(fMapReadLatency + PELatency).resized

  io.conv1DDone.setWhen(io.readDone & io.layerReadDone)
  io.conv1DDone.clearWhen(io.loadConfig)
  io.willOverflowIfInc := ow.willOverFlowIfInc & oh.willOverFlowIfInc & kt.willOverFlowIfInc
}
