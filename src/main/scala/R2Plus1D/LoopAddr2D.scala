package R2Plus1D
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.language.postfixOps
import Chainsaw._

case class LoopAddr2D(uc: Int = Parameter.Uc) extends Component {
  val loadLatency = 4
  val io = new Bundle {
    val loadConfig:  Bool              = in Bool ()
    val configIn:    ConfigParaPorts2D = in(ConfigParaPorts2D())
    val loadCounter: Bool              = out Bool ()
    val configReg:   ConfigParaPorts2D = out(ConfigParaPorts2D())

    val KrsTUc:                        UInt = out(RegNext(configReg.Krs * uc))
    val kSizeTUc:                      UInt = out(RegNext(configReg.kernelSize * uc))
    val NicDUicCeiTKSizeTUc:           UInt = out(RegNext(configReg.NicDUicCeil * kSizeTUc))
    val NcDUcCeilTNicDUicCeiTKSizeTUc: UInt = out(RegNext(configReg.NcDUcCeil * NicDUicCeiTKSizeTUc))

    val NdTTow:                UInt = out(RegNext(configReg.Nd * configReg.Tow))
    val NihwTNd:               UInt = out(RegNext(configReg.Nihw * configReg.Nd))
    val NihwTNdTTow:           UInt = out(RegNext(configReg.Tow * NihwTNd))
    val NdTKrs:                UInt = out(RegNext(configReg.Nd * configReg.Krs))
    val NihwTNdTKrs:           UInt = out(RegNext(configReg.Nihw * NdTKrs))
    val NihwTNdTToh:           UInt = out(RegNext(configReg.Toh * NihwTNd))
    val ifMapSizeTNicDUicCeil: UInt = out(RegNext(configReg.ifMapSize * configReg.NicDUicCeil))

    val NohwTNd:            UInt = out(RegNext(configReg.Nohw * configReg.Nd))
    val TohTNohwTNd:        UInt = out(RegNext(configReg.Toh * NohwTNd))
    val NohwTNohwTNd:       UInt = out(RegNext(configReg.Nohw * NohwTNd))
    val ofMapSizeNcDUcCeil: UInt = out(RegNext(configReg.ofMapSize * configReg.NcDUcCeil))
  }
  io.loadCounter := io.loadConfig.d(4)
  io.configReg.assignAllByName(RegNextWhen(io.configIn, io.loadConfig))

}
