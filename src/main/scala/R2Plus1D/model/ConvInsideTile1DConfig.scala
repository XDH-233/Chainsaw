package R2Plus1D.model

case class ConvInsideTile1DConfig(
    val Uic:     Int,
    val Uc:      Int,
    val Uoc:     Int,
    val Tc:      Int,
    val Noc:     Int,
    val Nid:     Int,
    val Nhw:     Int,
    val Nod:     Int,
    val Kt:      Int,
    val stride:  Int,
    val padding: Int
) {
  require((Nid + 2 * padding - Kt) / stride + 1 == Nod)
}
