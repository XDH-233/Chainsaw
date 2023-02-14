package R2Plus1D.model

import scala.language.postfixOps
import scala.tools.nsc.interactive.ContextTrees

case class ConvConfig(
    val Uic:      Int            = 36,
    val Nic:      Int            = 64,
    val Uc:       Int            = 144,
    val Uoc:      Int            = 26,
    val Nc:       Int            = 144,
    val Noc:      Int            = 512,
    val Nid:      Int            = 16,
    val Nihw:     Int            = 112,
    val K:        Int            = 3,
    val stride:   Int            = 1,
    val padding:  Int            = 1,
    val convType: ConvType.Value = ConvType.D2,
    val Toh:      Int            = 7,
    val Tow:      Int            = 7
) {

  val Nod: Int = convType match {
    case ConvType.D0 => (Nid + padding * 2 - K) / stride + 1
    case ConvType.D1 => (Nid + padding * 2 - K) / stride + 1
    case ConvType.D2 => Nid
  }
  val Nohw = convType match {
    case ConvType.D0 => (Nihw + padding * 2 - K) / stride + 1
    case ConvType.D1 => Nihw
    case ConvType.D2 => (Nihw + padding * 2 - K) / stride + 1
  }

  val ifMapSize: Int = Nihw * Nihw * Nid
  val ofMapSize: Int = Nod * Nohw * Nohw
  val kernelSize: Int = convType match {
    case ConvType.D0 => K
    case ConvType.D1 => K
    case ConvType.D2 => K * K * K
  }

  val NicDUicCeil: Int = (Nic.toDouble / Uic.toDouble).ceil.toInt
  val NcDUcCeil:   Int = (Nc.toDouble / Uc.toDouble).ceil.toInt
  val NocDUocCeil: Int = (Noc.toDouble / Uoc.toDouble).ceil.toInt

  val NohwDTohCeil = (Nohw.toDouble / Toh.toDouble).ceil.toInt
  val NohwDTowCeil = (Nohw.toDouble / Tow.toDouble).ceil.toInt

  def display(): Unit = {

    convType match {
      case ConvType.D0 => {
        printf(f"Uic: $Uic%-4d Nc: $Nic%-4d Uoc: $Uoc%-4d Noc: $Noc%-4d\n")
      }
      case ConvType.D1 => {
        printf(f"Uc: $Uc%-4d Nc: $Nc%-4d Uoc: $Uoc%-4d Noc: $Noc%-4d\n")
      }
      case ConvType.D2 => {
        printf(f"Uic: $Uic%-4d Nic: $Nic%-4d Uc: $Uc%-4d Nc: $Nc%-4d\n")
        printf(f"Tow: $Tow%-4d Toh: $Toh%-4d\n")
      }
    }
    printf(f"Nid: $Nid%-4d Nod: $Nod%-4d Nihw: $Nihw%-4d Nohw :$Nohw%-4d\n")
    printf(f"K: $K%-4d stride: $stride%-4d padding: $padding%-4d\n")
  }
}
