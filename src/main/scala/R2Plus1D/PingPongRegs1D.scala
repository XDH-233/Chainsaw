package R2Plus1D
import spinal.core._
import spinal.lib.fsm._

import scala.language.postfixOps
import Chainsaw._

case class PingPongRegs1D(dataWidth: Int = 8, uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val weightRdy:         Bool      = in Bool ()
    val loadConfig:        Bool      = in Bool ()
    val shortCut:          Bool      = in Bool ()
    val layerDone:         Bool      = in Bool ()
    val readDone:          Bool      = out Bool ()
    val readEn:            Bool      = out Bool () setAsReg () init False
    val weightFilled:      Bool      = out Bool () setAsReg () init False
    val ofMapSize2D:       UInt      = in UInt (log2Up(3136 + 1) bits)
    val weightLoadNum:     UInt      = in UInt (log2Up(uoc + 1) bits)
    val weightAddrBase:    UInt      = in UInt (log2Up(Parameter.weightBuffer1DDepth) bits)
    val weightAddr:        UInt      = out UInt (log2Up(Parameter.weightBuffer1DDepth) bits)
    val weightIn:          Bits      = in Bits (dataWidth * uc bits)
    val weightOut:         Vec[Bits] = out Vec (Bits(dataWidth * uc bits), uoc)
    val ofMap2DRdy:        Bool      = in Bool ()
    val fMapDDRRdy:        Bool      = in Bool ()
    val toWriteDone:       Bool      = in Bool ()
    val willOverFlowIfInc: Bool      = in Bool ()
  }
  io.readDone.clear()

  val configLoaded: Bool = Bool() setAsReg () init (False)
  when(io.loadConfig) {
    configLoaded.set()
  }
  val ping, pong:  Vec[Bits]      = Vec(Reg(Bits(dataWidth * uc bits)), uoc)
  val fillPing:    Bool           = RegInit(False)
  val load:        Bool           = io.readEn.d(readLatency)
  val readCounter: GeneralCounter = GeneralCounter(step = U(1), top = io.ofMapSize2D, en = io.loadConfig)

  Function.pingPongLoadAndOut(ping, pong, io.weightIn, io.weightOut, fillPing, load)
  val fMapFilled: Bool = RegInit(False)
  val FSM = new StateMachine {
    val idle     = new State with EntryPoint
    val fillOne  = new State
    val fillIn1D = new State
    val filled   = new State

    idle.whenIsActive {
      when(io.weightRdy & ~io.layerDone & configLoaded) {
        io.readEn.set()
        goto(fillOne)
      }

    }

    fillOne.whenIsActive {
      when(readCounter.value + 1 < io.weightLoadNum) {
        readCounter.inc()
      } otherwise {
        io.readEn.clear()
        when(io.shortCut & io.fMapDDRRdy) {
          io.readDone.set()
          readCounter.clear()
          io.readEn.set()
          io.weightFilled.set()
          goto(filled)
        } elsewhen (io.toWriteDone) {
          io.readDone.set()
          readCounter.clear()
          io.readEn.set()
          fMapFilled.set()
          goto(fillIn1D)
        }
      }
    }

    fillIn1D.whenIsActive {
      when(io.toWriteDone) {
        fMapFilled.set()
      } elsewhen (io.willOverFlowIfInc) {
        fMapFilled.clear()
      }
      when(~io.willOverFlowIfInc & fMapFilled) {
        readCounter.inc()
      }
      when(readCounter.willOverFlow) {
        io.readDone.set()
      }

      when(readCounter.value + 1 === io.weightLoadNum) {
        io.readEn.clear()
      } elsewhen (readCounter.willOverFlow) {
        io.readEn.set()
      }

      when(readCounter.value + 1 === readLatency) {
        fillPing := ~fillPing
      }

      when(io.ofMap2DRdy) {
        readCounter.clear()
        io.readEn.set()
        io.readDone.set()
        io.weightFilled.set()
        goto(filled)
      }
    }

    filled.whenIsActive {
      readCounter.inc()
      when(readCounter.value + 1 === io.weightLoadNum) {
        io.readEn.clear()
      } elsewhen (readCounter.willOverFlow) {
        io.readEn.set()
      }
      when(readCounter.value + 1 === readLatency) {
        fillPing := ~fillPing
      }
      when(readCounter.willOverFlow) {
        io.readDone.set()
      }

      when(io.layerDone & io.readDone) {
        load.clear()
        io.readEn.clear()
        io.weightFilled.clear()
        configLoaded.clear()
        goto(idle)
      }
    }
  }
  io.weightAddr := (io.weightAddrBase + (U(uoc) - readCounter.value)).resized
}
