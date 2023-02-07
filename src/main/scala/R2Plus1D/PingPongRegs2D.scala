package R2Plus1D

import spinal.core._
import spinal.lib.fsm._
import Chainsaw._
import Parameter._

import scala.language.postfixOps

case class PingPongRegs2D(uic: Int = 36, uoc: Int = 144, width: Int = 8, readLatency: Int = 4) extends Component {
  val io = new Bundle {
    val weightBufferRdy:    Bool      = in Bool ()
    val weightLoadedNum:    UInt      = in UInt (16 bits) //
    val weightAddrBase:     UInt      = in UInt (log2Up(weightBuffer2DDepth) bits)
    val tileDone:           Bool      = in Bool ()
    val layerDone:          Bool      = in Bool ()
    val weightIn:           Bits      = in Bits (uic * width bits)
    val weightOut:          Vec[Bits] = out Vec (Bits(uic * width bits), uoc)
    val readAddr:           UInt      = out UInt (log2Up(weightBuffer2DDepth) bits)
    val weightBufferReadEn: Bool      = out Bool ()
  }

  val state:    Bool = RegInit(False) // 0 -> read regs0, 1 -> read regs1
  val stateNxt: Bool = False.allowOverride

  stateNxt := state
  state    := stateNxt
  val regs: Seq[Vec[Bits]] = Seq.fill(2)(Vec(Reg(Bits(uic * width bits)), uoc))

  when(stateNxt & ~state) { // state turn to 1 will read regs1, clear regs0
    regs.head.foreach(_.clearAll())
  }
  when(state & ~stateNxt) { // state turn to 0, will read regs0, clear reg1
    regs.last.foreach(_.clearAll())
  }

  val readCounter: UInt = RegInit(U(0, 16 bits))
  val rdEn:        Bool = RegInit(False)
  val load:        Bool = rdEn.d(readLatency)
  when(rdEn) {
    readCounter := readCounter + 1
  }
  when(readCounter + 1 >= io.weightLoadedNum) {
    rdEn.clear()
  }

  io.readAddr := io.weightAddrBase + io.weightLoadedNum - readCounter
  io.weightOut.zip(regs.head).zip(regs.last).foreach { case ((w: Bits, ping: Bits), pong: Bits) => w := Mux(state, pong, ping) }
  when(state) { // 1 -> write regs0
    when(load) {
      regs.head.head := io.weightIn
      regs.head.init.zip(regs.head.tail).foreach { case (i, t) => t := i }
    }
  } otherwise { // 0 -> write regs1
    when(load) {
      regs.last.head := io.weightIn
      regs.last.init.zip(regs.last.tail).foreach { case (i, t) => t := i }
    }
  }

  val FSM = new StateMachine {
    val void    = new State with EntryPoint
    val fillOne = new State
    val fillTwo = new State
    val full    = new State
    void.whenIsActive {
      when(io.weightBufferRdy) {
        rdEn.set()
        stateNxt := ~state
        goto(fillOne)
      }
    }

    fillOne.whenIsActive {
      when(~rdEn.d(3) & load) {
        stateNxt := ~state
        rdEn.set()
        readCounter.clearAll()
        goto(fillTwo)
      }

    }
    fillTwo.whenIsActive {
      when(~rdEn.d(3) & load) {
        goto(full)
      }
    }

    full.whenIsActive {
      when(io.tileDone) {
        stateNxt := ~state
        readCounter.clearAll()
        rdEn.set()
      }
    }
  }
  io.weightBufferReadEn := rdEn
}
