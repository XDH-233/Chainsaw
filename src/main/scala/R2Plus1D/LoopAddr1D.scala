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

    // weight
    val ktTUoc:                      UInt = out(RegNext(configReg.Kt * uoc))
    val NcDUcCeilTKtUoc:             UInt = out(RegNext(configReg.NicDUicCeil * ktTUoc))
    val TocDUocCeilTNcDUcCeilTKtUoc: UInt = out(RegNext(configReg.TocDUocCeil * NcDUcCeilTKtUoc))

    val NohwTNid:              UInt = out(RegNext(configReg.Nid * configReg.Nohw))
    val NihwTNid:              UInt = out(RegNext(configReg.Nihw * configReg.Nid))
    val NohwTNihwTNid:         UInt = out(RegNext(configReg.Nohw * NihwTNid))
    val ifMapSizeTNicDUicCeil: UInt = out(RegNext(configReg.ifMapSize * configReg.NicDUicCeil))

    val NohwTNod:                          UInt = out(RegNext(configReg.Nohw * configReg.Nod))
    val ofMapSizeTTocDUocCeil:             UInt = out(RegNext(configReg.ofMapSize * configReg.TocDUocCeil))
    val NocDTocCeilTofMapSizeTTocDUocCeil: UInt = out(RegNext(configReg.NocDTcCeil * ofMapSizeTTocDUocCeil))

    val NocDUocCeilTUoc: UInt = out(RegNext(configReg.NocDUocCeil * uoc))
  }
  io.loadCounter := io.loadConfig.d(latency)
  io.configReg.assignAllByName(RegNextWhen(io.configIn, io.loadConfig))

}
