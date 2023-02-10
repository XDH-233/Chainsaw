package R2Plus1D
import spinal.core._
import spinal.lib.fsm._
import scala.language.postfixOps
import Chainsaw._

case class PingPongRegs1D(dataWidth: Int = 8, uc: Int = Parameter.Uc, uoc: Int = Parameter.Uoc, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val weightRdy:      Bool      = in Bool ()
    val loadConfig:     Bool      = in Bool ()
    val layerDone:      Bool      = in Bool ()
    val readDone:       Bool      = out Bool ()
    val readEn:         Bool      = out Bool () setAsReg () init False
    val weightFilled:   Bool      = out Bool () setAsReg () init False
    val ofMapSize2D:    UInt      = in UInt (log2Up(3136 + 1) bits)
    val weightLoadNum:  UInt      = in UInt (log2Up(Parameter.Uoc + 1) bits)
    val weightAddrBase: UInt      = in UInt (log2Up(Parameter.weightBuffer1Depth) bits)
    val weightAddr:     UInt      = out UInt (log2Up(Parameter.weightBuffer1Depth) bits)
    val weightIn:       Bits      = in Bits (dataWidth * uc bits)
    val weightOut:      Vec[Bits] = out Vec (Bits(dataWidth * uc bits), uoc)
  }
  io.readDone.clear()

  val ping, pong:  Vec[Bits]      = Vec(Reg(Bits(dataWidth * uc bits)), uoc)
  val fillPing:    Bool           = RegInit(False)
  val load:        Bool           = io.readEn.d(readLatency)
  val readCounter: GeneralCounter = GeneralCounter(step = U(1), top = io.ofMapSize2D, en = io.loadConfig)

  Function.pingPongLoadAndOut(ping, pong, io.weightIn, io.weightOut, fillPing, load)

  val FSM = new StateMachine {
    val idle    = new State with EntryPoint
    val fillOne = new State
    val filled  = new State

    idle.whenIsActive {
      when(io.weightRdy & ~io.layerDone) {
        io.readEn.set()
        goto(fillOne)
      }

    }

    fillOne.whenIsActive {
      readCounter.inc()
      when(readCounter.value + 1 === io.weightLoadNum) {
        io.readDone.set()
        readCounter.clear()
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
    }
  }
  io.weightAddr := (io.weightAddrBase + (U(uoc) - readCounter.value)).resized
}
