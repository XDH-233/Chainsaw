package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.language.postfixOps
import Chainsaw._

case class LoopAddr1D(uoc: Int = Parameter.Uoc) extends Component {
  val latency = 4
  val io = new Bundle {
    val loadConfig:  Bool              = in Bool ()
    val configIn:    ConfigParaPorts1D = in(ConfigParaPorts1D())
    val loadCounter: Bool              = out Bool ()
    val configReg:   ConfigParaPorts1D = out(ConfigParaPorts1D())

    val ktTUoc:                      UInt = out(RegNext(configReg.Kt * uoc))
    val NcDUcCeilTKtUoc:             UInt = out(RegNext(configReg.NcDUcCeil * ktTUoc))
    val NocDUocCeilTNcDUcCeilTKtUoc: UInt = out(RegNext(configReg.NocDUocCeil * NcDUcCeilTKtUoc))

    val NohwTNid:            UInt = out(RegNext(configReg.Nid * configReg.Nohw))
    val NihwTNid:            UInt = out(RegNext(configReg.Nihw * configReg.Nid))
    val NohwTNihwTNid:       UInt = out(RegNext(configReg.Nohw * NihwTNid))
    val ifMapSizeTNcDUcCeil: UInt = out(RegNext(configReg.ifMapSize * configReg.NcDUcCeil))

    val NohwTNod:              UInt = out(RegNext(configReg.Nohw * configReg.Nod))
    val NohwTNhowTNod:         UInt = out(RegNext(configReg.Nohw * NohwTNod))
    val ofMapSizeTNocDUocCeil: UInt = out(RegNext(configReg.ofMapSize * configReg.NocDUocCeil))
  }
  io.loadCounter := io.loadConfig.d(4)
  io.configReg.assignAllByName(RegNextWhen(io.configIn, io.loadConfig))

}
