package R2Plus1D.model

import scala.language.postfixOps

case class Conv1DConfig(
    val Uc:      Int,
    val Uoc:     Int,
    val Nc:      Int,
    val Noc:     Int,
    val Nid:     Int,
    val Nhw:     Int,
    val Kt:      Int,
    val stride:  Int,
    val padding: Int
) {
  val Nod:         Int = (Nid + padding * 2 - Kt) / stride + 1
  val ifMapSize:   Int = Nhw * Nhw * Nid
  val ofMapSize:   Int = Nod * Nhw * Nhw
  val NcDUcCeil:   Int = (Nc.toDouble / Uc.toDouble).ceil.toInt
  val NocDUocCeil: Int = (Noc.toDouble / Uoc.toDouble).ceil.toInt

  def display(): Unit = {
    printf(f"Uc: $Uc%-4d Nc: $Nc%-4d Uoc: $Uoc%-4d Noc: $Noc%-4d\n")
    printf(f"Nid: $Nid%-4d Nod: $Nod%-4d Nhw: $Nhw%-4d\n")
    printf(f"Kt: $Kt%-4d stride: $stride%-4d padding: $padding%-4d\n")
  }
}
