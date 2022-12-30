package R2Plus1D

case class ConvInsideTile2DConfig(
    val Uic:     Int,
    val Uc:      Int,
    val Tic:     Int,
    val Tc:      Int,
    val Nid:     Int,
    val Nihw:    Int,
    val Nod:     Int,
    val Nohw:    Int,
    val Krs:     Int,
    val stride:  Int,
    val padding: Int
) {
  require((Nihw + 2 * padding - Krs) / stride + 1 == Nohw)
}
