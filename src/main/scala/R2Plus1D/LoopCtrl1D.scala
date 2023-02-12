package R2Plus1D

import spinal.core._
import spinal.lib._
import scala.language.postfixOps
import Chainsaw._

case class LoopCtrl1D(uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc) extends Component {
  val io = new Bundle {
    val loadConfig = in Bool ()
    val config     = slave(new ConfigParaPorts1D())

    val weightAddrBase  = out UInt (log2Up(Parameter.weightBuffer1DDepth) bits)
    val weightLoadedNum = out(UInt(log2Up(uoc + 1) bits)) setAsReg () init 0
    val readDone        = in Bool ()
    val weightFilled    = in Bool ()
    val layerDone       = out Bool () setAsReg () init False
    val ifMapRdEn       = out Bool ()
    val ifMapAddr       = out UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val ifMapAddrVld    = out Bool ()

  }

  val ow    = GeneralCounter(step = U(1), top = io.config.Nhw, en = io.loadConfig)
  val oh    = GeneralCounter(step = U(1), top = io.config.Nhw, en = io.loadConfig)
  val kt    = GeneralCounter(step = U(1), top = io.config.Kt, en = io.loadConfig)
  val od    = GeneralCounter(step = U(1), top = io.config.Nod, en = io.loadConfig)
  val ti    = GeneralCounter(step = U(1), top = io.config.NcDUcCeil, en = io.loadConfig)
  val tiNc  = GeneralCounter(step = U(uc), top = io.config.Nc, en = io.loadConfig)
  val to    = GeneralCounter(step = U(1), top = io.config.NocDUocCeil, en = io.loadConfig)
  val toNoc = GeneralCounter(step = U(uoc), top = io.config.Noc, en = io.loadConfig)

  when(io.weightFilled) {
    ow.inc()
  }
  when(ow.willOverFlow) {
    oh.inc()
  }
  when(oh.willOverFlow) {
    kt.inc()
  }
  when(kt.willOverFlow) {
    od.inc()
  }
  when(od.willOverFlow) {
    ti.inc()
    tiNc.inc()
  }
  when(ti.willOverFlow) {
    to.inc()
    toNoc.inc()
  }

  // ----------------------weight addr base accumulation ----------------------------------------------------------------
  val ktWeightAddr = GeneralCounter(step = U(uoc), top = io.config.Kt * uoc, en = io.loadConfig)
  val tiWeightAddr = GeneralCounter(step = io.config.Kt * uoc, top = io.config.NcDUcCeil * io.config.Kt * uoc, io.loadConfig)
  val toWeightAddr = GeneralCounter(
    step = io.config.NcDUcCeil * io.config.Kt * uoc,
    top  = io.config.NocDUocCeil * io.config.NocDUocCeil * io.config.Kt * uoc,
    en   = io.loadConfig
  )
  when(io.readDone) {
    ktWeightAddr.inc()
  }
  when(ktWeightAddr.willOverFlow & od.willOverFlowIfInc) {
    tiWeightAddr.inc()
  }
  when(tiWeightAddr.willOverFlow) {
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
  io.layerDone.setWhen(toWeightAddr.willOverFlow)
  io.layerDone.clearWhen(io.loadConfig)
  // ---------------------- if Map addr accumulation -------------------------------------------------------------------
  val odIfMapAddr = GeneralCounter(step = Mux(io.config.stride, U(2), U(1)), top = Mux(io.config.stride, io.config.Nod << 1, io.config.Nod), en = io.loadConfig)
  val owIfMapAddr = GeneralCounter(step = io.config.Nid, top = io.config.Nid * io.config.Nhw, en = io.loadConfig)
  val ohIfMapAddr = GeneralCounter(step = io.config.Nhw * io.config.Nid, top = io.config.Nhw * io.config.Nid * io.config.Nhw, en = io.loadConfig)
  val tiIfMapAddr = GeneralCounter(step = io.config.ifMapSize, top = io.config.NcDUcCeil * io.config.ifMapSize, en = io.loadConfig)
  when(io.weightFilled) {
    odIfMapAddr.inc()
  }
  when(odIfMapAddr.willOverFlow) {
    owIfMapAddr.inc()
  }
  when(owIfMapAddr.willOverFlow) {
    ohIfMapAddr.inc()
  }
  when(ohIfMapAddr.willOverFlow) {
    tiIfMapAddr.inc()
  }
  val id = (Mux(io.config.stride, od.value << 1, od.value) + kt.value - io.config.padding).asSInt
  io.ifMapAddrVld := id >= 0 & id < io.config.Nid.asSInt
  io.ifMapRdEn    := io.weightFilled
  io.ifMapAddr    := (tiIfMapAddr.value + ohIfMapAddr.value + owIfMapAddr.value + odIfMapAddr.value + io.config.Kt - io.config.padding).resized
}
