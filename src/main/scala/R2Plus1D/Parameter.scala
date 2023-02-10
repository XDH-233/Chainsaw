package R2Plus1D
import spinal.core._
object Parameter {
  val Uic   = 36
  val Uc    = 144
  val Uoc   = 36
  val WIDTH = 8

  val featureMapDepth: Int = 100 * 1024 // 100k

  val weightBuffer2DDepth: Int = 152 * 1024 // 152k
  val weightBuffer1Depth:  Int = 16 * 1024 // 16k
  val outputBuffer2DDepth: Int = 208 * 4 * 1024

  val ifMapSizeMax2D: Int = 16 * 112 * 112
  val ifMapSizeMax1D: Int = 16 * 56 * 56

}
