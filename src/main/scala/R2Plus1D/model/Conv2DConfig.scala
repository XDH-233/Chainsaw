package R2Plus1D.model
import scala.math.sqrt
case class Conv2DConfig(
    val Uic:     Int,
    val Uc:      Int,
    val Nic:     Int,
    val Nc:      Int,
    val Nd:      Int,
    val Nihw:    Int,
    val Krs:     Int,
    val stride:  Int,
    val padding: Int
) {
  val Nohw:         Int = (Nihw + 2 * padding - Krs) / stride + 1
  val ifMapSize:    Int = Nihw * Nihw * Nd
  val kernelSize:   Int = Krs * Krs
  val NicDUicCeil:  Int = divideCeil(Nic, Uic)
  val NcDUcCeil:    Int = divideCeil(Nc, Uc)
  val Toh:          Int = sqrt(Uc / Nd).floor.toInt
  val Tow:          Int = Uc / Nd / Toh
  val NohwDTohCeil: Int = divideCeil(Nohw, Toh)
  val NohwDTowCeil: Int = divideCeil(Nohw, Tow)

  def divideCeil(a: Int, b: Int): Int = (a.toDouble / b.toDouble).ceil.toInt

  def display(): Unit = {
    printf(f"Nic: $Nic%-4d Uic: $Uic%-4d Nc: $Nc%-4d Uc: $Uc%-4d\n")
    printf(f"Nihw: $Nihw%-4d Nd: $Nd%-4d Nohw: $Nohw%-4d Toh: $Toh%-4d Tow: $Tow%-4d\n")
    printf(f"Krs: $Krs%-4d S: $stride%-4d P: $padding%-4d \n")
    printf(f"NcDUcCeil: $NcDUcCeil%-4d NicDUicCeil: $NicDUicCeil%-4d NohwDTohCeil: $NohwDTohCeil%-4d NohwDTowCeil: $NohwDTowCeil%-4d\n")

  }
}

object fuck extends App {
  println(sqrt(8 / 2).floor.toInt)
}
