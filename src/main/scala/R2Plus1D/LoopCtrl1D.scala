package R2Plus1D

import spinal.core._

import scala.language.postfixOps
import Chainsaw._

case class LoopCtrl1D(uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc) extends Component {
  val width = 16
  val io = new Bundle {
    val loadConfig = in Bool ()

    val Nhw                    = in UInt (width bits)
    val Kt                     = in UInt (2 bits)
    val Nid                    = in UInt (5 bits)
    val Nod                    = in UInt (5 bits)
    val Nc, Noc                = in UInt (width bits)
    val NcDUcCeil, NocDUocCeil = in UInt (8 bits)
    val stride                 = in Bool ()
    val padding                = in UInt (2 bits)
    val ifMapSize              = in UInt (log2Up(Parameter.ifMapSizeMax1D + 1) bits)

    val weightAddrBase  = out UInt (log2Up(Parameter.weightBuffer1Depth) bits)
    val weightLoadedNum = out(UInt(log2Up(uoc + 1) bits))
    val PEDone          = in Bool ()
    val readDone        = in Bool ()
    val weightFilled    = in Bool ()
    val ifMapRdEn       = out Bool ()
    val ifMapAddr       = out UInt (log2Up(Parameter.outputBuffer2DDepth) bits)
    val ifMapAddrVld    = out Bool ()

  }

  val ow    = GeneralCounter(step = U(1), top = io.Nhw, en = io.loadConfig)
  val oh    = GeneralCounter(step = U(1), top = io.Nhw, en = io.loadConfig)
  val kt    = GeneralCounter(step = U(1), top = io.Kt, en = io.loadConfig)
  val od    = GeneralCounter(step = U(1), top = io.Nod, en = io.loadConfig)
  val ti    = GeneralCounter(step = U(1), top = io.NcDUcCeil, en = io.loadConfig)
  val tiNc  = GeneralCounter(step = U(uc), top = io.Nc, en = io.loadConfig)
  val to    = GeneralCounter(step = U(1), top = io.NocDUocCeil, en = io.loadConfig)
  val toNoc = GeneralCounter(step = U(uoc), top = io.Noc, en = io.loadConfig)

  when(io.PEDone) {
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
  val ktWeightAddr = GeneralCounter(step = U(uoc), top = io.Kt * uoc, en = io.loadConfig)
  val tiWeightAddr = GeneralCounter(step = io.Kt * uoc, top = io.NcDUcCeil * io.Kt * uoc, io.loadConfig)
  val toWeightAddr = GeneralCounter(step = io.NcDUcCeil * io.Kt * uoc, top = io.NocDUocCeil * io.NocDUocCeil * io.Kt * uoc, io.loadConfig)
  // TODO
  io.weightAddrBase := (ktWeightAddr.value + tiWeightAddr.value + toWeightAddr.value).resized
  io.weightLoadedNum.clearAll() // FIXME
  // ---------------------- if Map addr accumulation -------------------------------------------------------------------
  val odIfMapAddr = GeneralCounter(step = Mux(io.stride, U(2), U(1)), top = Mux(io.stride, io.Nod << 1, io.Nod), en = io.loadConfig)
  val owIFMapAddr = GeneralCounter(step = io.Nid, top = io.Nid * io.Nhw, en = io.loadConfig)
  val ohIfMapAddr = GeneralCounter(step = io.Nhw * io.Nid, top = io.Nhw * io.Nid * io.Nhw, en = io.loadConfig)
  val tiIfMapAddr = GeneralCounter(step = io.ifMapSize, top = io.NcDUcCeil * io.ifMapSize, en = io.loadConfig)
  // TODO
  val id = (Mux(io.stride, od.value << 1, od.value) + io.Kt - io.padding).asSInt
  io.ifMapAddrVld := id >= 0 | id < io.Nid.asSInt
  io.ifMapRdEn    := io.weightFilled
  io.ifMapAddr    := (tiIfMapAddr.value + ohIfMapAddr.value + owIFMapAddr.value + odIfMapAddr.value + io.Kt - io.padding).resized
}
