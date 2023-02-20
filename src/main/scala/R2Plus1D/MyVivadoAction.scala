package R2Plus1D

import Chainsaw.synthWorkspace
import Chainsaw.xilinx._
import spinal.core._
import Chainsaw._

import java.io.File
import scala.language.postfixOps

object MyVivadoAction {
  def apply[T <: Module](design: => T, name: String, flowType: EdaFlowType): VivadoReport = {
    val flow =
      new VivadoFlow(design, flowType, XilinxDevice(UltraScale, "XCU250-FIGD2104-2L-E".toLowerCase, 300 MHz, None), name, new File(synthWorkspace, name))
    flow.doFlow()
  }
}
