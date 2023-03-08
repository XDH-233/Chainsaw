package R2Plus1D
import spinal.core._
object Parameter {
  val Uic   = 36
  val Uc    = 144
  val Uoc   = 36
  val WIDTH = 8

  val NicMax = 512
  val NcMax  = 1152
  val NocMax = 512

  val NihwMax2D = 112
  val NohwMax2D = 56
  val NihwMax1D = 56
  val NohwMax1D = 56
  val featureMapDepth: Int = 25 * 4 * 1024

  val weightBuffer2DDepth: Int = 41472
  val weightBuffer1DDepth: Int = 6912
  val outputBuffer2DDepth: Int = 13 * 4 * 1024

  val ifMapSizeMax2D: Int = 16 * 112 * 112
  val ifMapSizeMax1D: Int = 16 * 56 * 56
  val ofMapSizeMax2D: Int = ifMapSizeMax1D
  val ofMapSizeMax1D: Int = 50176
  val ofMapMaxOwOhSize1D = 3136
}
