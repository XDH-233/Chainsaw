package R2Plus1D.model

case class ConvInsideTile2DConfig( // FIXME
    val Uic:     Int,
    val Uc:      Int,
    val Nic:     Int,
    val Tc:      Int,
    val Nd:      Int,
    val Nihw:    Int,
    val Nohw:    Int,
    val Krs:     Int,
    val stride:  Int,
    val padding: Int
) {
  require((Nihw + 2 * padding - Krs) / stride + 1 == Nohw)
}
