package Chainsaw.xilinx

import Chainsaw._
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib._

import java.io.File
import scala.collection.mutable
import scala.io.Source
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

/** used to generate sources for a Vivado flow and invoke Vivado to run it
  */
class VivadoFlow[T <: Component](
    design:           => T,
    taskType:         EdaFlowType,
    xilinxDevice:     XilinxDevice,
    topModuleName:    String,
    workspaceDir:     File,
    xdcFile:          Option[File]         = None,
    netlistDir:       Option[File]         = None,
    customizedConfig: Option[SpinalConfig] = None
) {

  require(hasVivado, "to use VivadoFlow, please set the environment variable 'VIVADO' to the vivado executable, e.g. /tools/Xilinx/Vivado/2022.1/bin/vivado ")

  // TODO: for windows
  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  /** core function of Vivado flow, used to run a synth/impl task
    *
    * @return
    *   VivadoReport as an object
    */
  def doFlow(): VivadoReport = {
    atSimTime = false // synth, rather than sim

    // create workspace directory
    workspaceDir.mkdirs()
    // files used in the flow
    val tclFile       = new File(workspaceDir, "doit.tcl")
    val simpleXdcFile = new File(workspaceDir, "doit.xdc")
    val logFile       = new File(workspaceDir, "doit.log")

    // generate RTL sources from dut
    // TODO: use netlistDir instead of netlistFile

    val config = customizedConfig match {
      case Some(value) => value
      case None => // for general Component
        val config = SpinalConfig(
          defaultConfigForClockDomains = xilinxCDConfig,
          targetDirectory              = workspaceDir.getAbsolutePath + "/",
          oneFilePerComponent          = true
        )
        config.addTransformationPhase(new phases.FfIo)
    }

    val rtlResources: Seq[String] = netlistDir match {
      case Some(src) =>
        FileUtils.copyDirectory(src, workspaceDir)
        val postfixes = Seq(".v", ".sv", ".vhd", ".vhdl")
        workspaceDir
          .listFiles()
          .toSeq
          .map(_.getAbsolutePath)
          .filter(path => postfixes.exists(postfix => path endsWith postfix))
      case None =>
        config.generateVerilog(design.setDefinitionName(topModuleName))
        val targetDir = new File(config.targetDirectory)
        if (customizedConfig.isDefined) {
          FileUtils.copyDirectory(targetDir, workspaceDir)
        }
        val lstFile =
          Source.fromFile(new File(workspaceDir, s"$topModuleName.lst"))
        val ret = lstFile
          .getLines()
          .map { line => new File(line) }
          .map(_.getAbsolutePath)
          .toSeq
        ret
    }

    // generate xdc & tcl file
    val simpleLine = {
      val targetPeriod = xilinxDevice.fMax.toTime
      s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
    }
    FileUtils.write(simpleXdcFile, simpleLine)

    // priority: specific xdc > device xdc > simple xdc
    val xdcCandidates = Seq(xdcFile, xilinxDevice.xdcFile, Some(simpleXdcFile))
    val xdcFileInUse  = xdcCandidates.flatten.head

    FileUtils.write(tclFile, getTcl(rtlResources, xdcFileInUse))

    // run vivado
    DoCmd.doCmd(
      s"${vivadoPath.getAbsolutePath} -stack 2000 -nojournal -log ${logFile.getAbsolutePath} -mode batch -source ${tclFile.getAbsolutePath}",
      workspaceDir.getAbsolutePath
    )

    /** -------- report
      * --------
      */
    val report =
      new VivadoReport(
        logFile,
        xilinxDevice.family
      ) // parse log file to get report
    logger.info(s"\n----vivado flow report----\n" + report.toString)
    atSimTime = true
    report
  }

  /** generate tcl script content for Vivado Flow
    *
    * @param dutRtlSources
    *   paths of rtl sources generated by dut
    * @return
    *   context of tcl script
    */
  def getTcl(dutRtlSources: Seq[String], xdcFile: File): String = {
    var script = ""

    /** rtl file path -> read command
      */
    def getReadCommand(sourcePath: File): String = {
      if (sourcePath.getPath.endsWith(".sv")) s"read_verilog -sv $sourcePath \n"
      else if (sourcePath.getPath.endsWith(".v")) s"read_verilog $sourcePath \n"
      else if (
        sourcePath.getPath
          .endsWith(".vhdl") || sourcePath.getPath.endsWith(".vhd")
      ) s"read_vhdl $sourcePath \n"
      else if (sourcePath.getPath.endsWith(".bin")) "\n"
      else
        throw new IllegalArgumentException(
          s"invalid RTL source path $sourcePath"
        )
    }

    dutRtlSources.foreach(path => script += getReadCommand(new File(path)))

    // read constraint sources
    script += s"read_xdc ${xdcFile.getAbsolutePath}\n"

    // do flow
    def addSynth(): Unit = {
      script += s"synth_design -part ${xilinxDevice.part} -top $topModuleName -mode out_of_context -retiming\n"
      script += s"synth_design -part ${xilinxDevice.part} -top $topModuleName -mode out_of_context\n"
      script += s"write_checkpoint -force ${topModuleName}_after_synth.dcp\n"
      script += s"report_timing\n"
    }

    def addImpl(): Unit = {
      script += "opt_design\n"
      script += "place_design -directive Explore\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${topModuleName}_after_place.dcp\n"
      script += "phys_opt_design\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${topModuleName}_after_place_phys_opt.dcp\n"
      script += "route_design\n"
      script += s"write_checkpoint -force ${topModuleName}_after_route.dcp\n"
      script += "report_timing\n"
      script += "phys_opt_design\n"
      script += "report_timing\n"
      script += s"write_checkpoint -force ${topModuleName}_after_route_phys_opt.dcp\n"
    }

    taskType match {
      case SYNTH =>
        addSynth()
      case IMPL =>
        addSynth()
        addImpl()
    }
    // util & timing can't be reported after synth/impl
    script += s"report_utilization -hierarchical -hierarchical_depth 10\n"
    //    script += s"report_utilization\n"
    script
  }
}

object VivadoFlow {
  def apply[T <: Module](
      design:        => T,
      taskType:      EdaFlowType,
      xilinxDevice:  XilinxDevice,
      topModuleName: String,
      workspacePath: File,
      xdcFile:       Option[File]
  ): VivadoFlow[T] =
    new VivadoFlow(
      design,
      taskType,
      xilinxDevice,
      topModuleName,
      workspacePath,
      xdcFile
    )
}

object DefaultVivadoFlow {
  def general[T <: Module](
      design:      => T,
      name:        String,
      flowType:    EdaFlowType,
      netlistFile: Option[File],
      config:      Option[SpinalConfig]
  ) = {
    val flow = new VivadoFlow(
      design,
      flowType,
      vu9p,
      name,
      new File(synthWorkspace, name),
      netlistDir       = netlistFile,
      customizedConfig = config
    )
    flow.doFlow()
  }
}

object VivadoSynth {
  def apply[T <: Module](design: => T, name: String) =
    DefaultVivadoFlow.general(design, name, SYNTH, None, None)

  def apply[T <: Module](design: => T, name: String, config: SpinalConfig) =
    DefaultVivadoFlow.general(design, name, SYNTH, None, Some(config))

  def apply[T <: Module](netlistDir: File, topModuleName: String) =
    DefaultVivadoFlow.general(
      null,
      topModuleName,
      SYNTH,
      Some(netlistDir),
      None
    )
}

object VivadoImpl {
  def apply[T <: Module](design: => T, name: String) =
    DefaultVivadoFlow.general(design, name, IMPL, None, None)

  def apply[T <: Module](design: => T, name: String, config: SpinalConfig) =
    DefaultVivadoFlow.general(design, name, IMPL, None, Some(config))

  def apply[T <: Module](netlistFile: File, topModuleName: String) =
    DefaultVivadoFlow.general(
      null,
      topModuleName,
      IMPL,
      Some(netlistFile),
      None
    )
}
