package R2Plus1D
import spinal.core._
object Parameter {
  val Uic = 64
  val Uc  = 128
  val Uoc = 48
  val featureMapBufferAddrWidth: Int = log2Up(49 * 1024) // 49 K
  val weightBuffer2DAddrWidth:   Int = log2Up(20736) // 20.25 K
  val outputBufferAddrWidth:     Int = log2Up(56448) // 55.125 K
}
