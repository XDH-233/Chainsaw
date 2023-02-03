package R2Plus1D

import spinal.core._
import Parameter._

case class Conv2Plus1D() extends Component {
  val io = new Bundle {}

  val PE2D: PE = PE(uic = Uic, uoc = Uc, width = WIDTH)
  val PE1D: PE = PE(uic = Uc, uoc = Uoc, width = WIDTH)

  val weightBuffer2D: WeightBuffer = WeightBuffer(dataWidth = WIDTH, weightBuffer2DDepth)

  val featureMapBuffer: FeatureMapBuffer = FeatureMapBuffer(width = WIDTH, depth = featureMapDepth)

}
