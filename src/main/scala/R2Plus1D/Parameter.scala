package R2Plus1D
import spinal.core._
object Parameter {
  val Uic   = 64
  val Uc    = 72
  val Uoc   = 64
  val WIDTH = 8

  val NicMax = 512
  val NcMax  = 1152
  val NocMax = 512

  val NihwMax2D = 112
  val NohwMax2D = 56
  val NihwMax1D = 56
  val NohwMax1D = 56
  val featureMapDepth: Int = 50176

  val weightBuffer2DDepth: Int = 20736
  val weightBuffer1DDepth: Int = 12288
  val outputBuffer2DDepth: Int = 100352

  val ifMapSizeMax2D: Int = 16 * 112 * 112
  val ifMapSizeMax1D: Int = 16 * 56 * 56
  val ofMapSizeMax2D: Int = ifMapSizeMax1D
  val ofMapSizeMax1D: Int = 50176
  val ofMapMaxOwOhSize1D = 3136
}
