package R2Plus1D

import Chainsaw.synthWorkspace
import Chainsaw.xilinx._
import spinal.core.Module

import java.io.File




object MyVivadoAction{
  def apply[T <: Module](design: => T, name: String, flowType: EdaFlowType): VivadoReport = {
    val flow = new VivadoFlow(design, flowType, u250, name, new File(synthWorkspace, name), netlistFile = None)
  flow.doFlow()
  }
}