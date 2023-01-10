package R2Plus1D.model

case class Conv2DConfig(
    val Uic:     Int,
    val Uc:      Int,
    val Nic:     Int,
    val Nc:      Int,
    val Nd:      Int,
    val Nihw:    Int,
    val Nohw:    Int,
    val Krs:     Int,
    val stride:  Int,
    val padding: Int
) {
  require((Nihw + 2 * padding - Krs) / stride + 1 == Nohw, "err in the input and output feature map size!")
}
