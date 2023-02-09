package R2Plus1D

import spinal.core._
import spinal.lib.fsm._
import spinal.lib._
import Chainsaw._
import Parameter._

import scala.language.postfixOps

case class PingPongRegs2D(uic: Int = 36, uoc: Int = 144, width: Int = 8, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val weightBufferRdy: Bool      = in Bool ()
    val weightLoadedNum: UInt      = in UInt (log2Up(uoc) bits) //
    val weightAddrBase:  UInt      = in UInt (log2Up(weightBuffer2DDepth) bits)
    val tileDone:        Bool      = out Bool () setAsReg () init (False)
    val layerDone:       Bool      = in Bool ()
    val weightIn:        Bits      = in Bits (uic * width bits)
    val weightOut:       Vec[Bits] = out Vec (Bits(uic * width bits), uoc)
    val readAddr:        UInt      = out UInt (log2Up(weightBuffer2DDepth) bits)
    val readEn:          Bool      = out Bool () setAsReg () init (False)
    val readDone:        Bool      = out Bool ()
    val filled:          Bool      = out Bool () setAsReg () init (False)
  }

  val loadPing:    Bool      = RegInit(False)
  val readCounter: Counter   = Counter(0, uoc - 1)
  val load:        Bool      = io.readEn.d(readLatency)
  val ping, pong:  Vec[Bits] = Vec(Reg(Bits(uic * width bits)), uoc)

  io.weightOut.zip(ping).zip(pong).foreach { case ((w, i), o) => w := Mux(loadPing, o, i) }
  when(loadPing) {
    loadShift(ping)
  } otherwise {
    loadShift(pong)
  }

  val FSM = new StateMachine {
    val idle    = new State with EntryPoint
    val fillOne = new State
    val filled  = new State

    idle.whenIsActive {
      when(io.weightBufferRdy) {
        io.readEn.set()
        pong.foreach(_.clearAll())
        goto(fillOne)
      }

      fillOne.whenIsActive {
        readCounter.increment()
        when(readCounter.willOverflow) {
          io.filled.set()
          goto(filled)
        }
      }

      filled.whenIsActive {
        readCounter.increment()
        when(readCounter.valueNext >= io.weightLoadedNum) {
          io.readEn.clear()
        } otherwise {
          io.readEn.set()
        }
        when(readCounter.value === readLatency - 1) {
          loadPing := ~loadPing
          when(loadPing) { // ping is loading and pong need to be cleared
            pong.foreach(_.clearAll())
          } otherwise {
            ping.foreach(_.clearAll())
          }
        }
      }

    }
  }

  io.readAddr := io.weightAddrBase + (U(uoc - 1) - readCounter.value)
  io.readDone := readCounter.willOverflow
  def loadShift(p: Vec[Bits]): Unit = {
    when(load) {
      p.head := io.weightIn
      p.tail.zip(p.init).foreach { case (t, i) => t := i }
    }
  }
}
