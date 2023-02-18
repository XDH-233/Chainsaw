package Chainsaw.memory
import spinal.core._

import scala.language.postfixOps
import Chainsaw.memory.ReadUnderWriteMode._

case class SPURAM(width: Int, depth: Int, pipeRegsCount: Int, singlePortMode: ReadUnderWriteMode.Value) extends BlackBox {
  val io = new Bundle {
    val clk:    Bool = in Bool () // Clock
    val reset:  Bool = in Bool ()
    val we:     Bool = in Bool () // Write Enable
    val mem_en: Bool = in Bool () // Memory Enable
    val din:    Bits = in Bits (width bits) // Data Input
    val addr:   UInt = in UInt (log2Up(depth) bits) // Address Input
    val dout:   Bits = out Bits (width bits) // Data Output
  }

  noIoPrefix()

  addGenerics(("DWIDTH", width), ("DEPTH", depth), ("NBPIPE", pipeRegsCount - 1))
  mapClockDomain(clock = io.clk, reset = io.reset)

  def read(en: Bool, addr: UInt, data: Bits) = {
    io.mem_en := en
    io.we.clear()
    io.din.clearAll()
    io.addr := addr
    data    := io.dout
  }

  def write(en: Bool, we: Bool, addr: UInt, data: Bits) = {
    io.mem_en := en
    io.we     := we
    io.din    := data
    io.addr   := addr
  }

  singlePortMode match {
    case NO_CHANGE => setInlineVerilog("""
                                           |module SPURAM(
                                           |    input clk,                    // Clock 
                                           |input reset,                   // no use, just for simulation
                                           |input we,                     // Write Enable
                                           |input mem_en,                 // Memory Enable
                                           |input[DWIDTH-1:0] din,       // Data Input  
                                           |input[$clog2(DEPTH)-1:0] addr,      // Address Input
                                           |output reg [DWIDTH-1:0] dout               // Data Output
                                           |);
                                           |//  Xilinx UltraRAM Single Port No Change Mode.  This code implements 
                                           |//  a parameterizable UltraRAM block in No Change mode. The behavior of this RAM is 
                                           |//  when data is written, the output of RAM is unchanged. Only when write is
                                           |//  inactive data corresponding to the address is presented on the output port.
                                           |
                                           |parameter DEPTH = 4096;  // Depth
                                           |parameter DWIDTH = 72;  // Data Width
                                           |parameter NBPIPE = 3;   // Number of pipeline Registers
                                           |
                                           |
                                           |
                                           |(* ram_style = "ultra" *)
                                           |reg [DWIDTH-1:0] mem[DEPTH-1:0];        // Memory Declaration
                                           |reg [DWIDTH-1:0] memreg;              
                                           |reg [DWIDTH-1:0] mem_pipe_reg[NBPIPE-1:0];    // Pipelines for memory
                                           |reg mem_en_pipe_reg[NBPIPE:0];                // Pipelines for memory enable  
                                           |
                                           |integer          i;
                                           |
                                           |// RAM : Both READ and WRITE have a latency of one
                                           |always @ (posedge clk)
                                           |begin
                                           | if(mem_en) 
                                           |  begin
                                           |   if(we)
                                           |    mem[addr] <= din;
                                           |   else
                                           |    memreg <= mem[addr];
                                           |  end
                                           |end
                                           |
                                           |// The enable of the RAM goes through a pipeline to produce a
                                           |// series of pipelined enable signals required to control the data
                                           |// pipeline.
                                           |always @ (posedge clk)
                                           |begin
                                           | mem_en_pipe_reg[0] <= mem_en;
                                           | for (i=0; i<NBPIPE; i=i+1)
                                           |   mem_en_pipe_reg[i+1] <= mem_en_pipe_reg[i];
                                           |end
                                           |
                                           |// RAM output data goes through a pipeline.
                                           |always @ (posedge clk)
                                           |begin
                                           | if (mem_en_pipe_reg[0])
                                           |  mem_pipe_reg[0] <= memreg;
                                           |end    
                                           |
                                           |always @ (posedge clk)
                                           |begin
                                           | for (i = 0; i < NBPIPE-1; i = i+1)
                                           |  if (mem_en_pipe_reg[i+1])
                                           |   mem_pipe_reg[i+1] <= mem_pipe_reg[i];
                                           |end      
                                           |
                                           |always @ (posedge clk)
                                           |begin
                                           | if (mem_en_pipe_reg[NBPIPE] )
                                           |   dout <= mem_pipe_reg[NBPIPE-1];
                                           |end
                                           |endmodule""".stripMargin)
    case READ_FIRST => setInlineVerilog("""module SPURAM(
                                           |input clk,                    // Clock 
                                           |input reset,
                                           |input we,                    // Write Enable
                                           |input mem_en,                 // Memory Enable
                                           |input[DWIDTH-1:0] din,       // Data Input  
                                           |input[$clog2(DEPTH)-1:0] addr,      // Address Input
                                           |output reg [DWIDTH-1:0] dout                // Data Output
                                           |
                                           |);
                                           |
                                           |
                                           |
                                           |//  Xilinx UltraRAM Single Port Read First Mode.  This code implements 
                                           |//  a parameterizable UltraRAM block in read first mode. The behavior of this RAM is 
                                           |//  when data is written, the old memory contents at the write address are 
                                           |//  presented on the output port.
                                           |//
                                           |parameter  DEPTH = 4096;  // DEPTH
                                           |parameter DWIDTH = 72;  // Data Width
                                           |parameter NBPIPE = 3;   // Number of pipeline Registers
                                           |
                                           |
                                           |(* ram_style = "ultra" *)
                                           |reg [DWIDTH-1:0] mem[DEPTH-1:0];        // Memory Declaration
                                           |reg [DWIDTH-1:0] memreg;              
                                           |reg [DWIDTH-1:0] mem_pipe_reg[NBPIPE-1:0];    // Pipelines for memory
                                           |reg mem_en_pipe_reg[NBPIPE:0];                // Pipelines for memory enable  
                                           |
                                           |integer          i;
                                           |
                                           |// RAM : Both READ and WRITE have a latency of one
                                           |always @ (posedge clk)
                                           |begin
                                           | if(mem_en) 
                                           |  begin
                                           |   if(we)
                                           |    mem[addr] <= din;
                                           |   memreg <= mem[addr];
                                           |  end
                                           |end
                                           |
                                           |// The enable of the RAM goes through a pipeline to produce a
                                           |// series of pipelined enable signals required to control the data
                                           |// pipeline.
                                           |always @ (posedge clk)
                                           |begin
                                           | mem_en_pipe_reg[0] <= mem_en;
                                           | for (i=0; i<NBPIPE; i=i+1)
                                           |   mem_en_pipe_reg[i+1] <= mem_en_pipe_reg[i];
                                           |end
                                           |
                                           |// RAM output data goes through a pipeline.
                                           |always @ (posedge clk)
                                           |begin
                                           | if (mem_en_pipe_reg[0])
                                           |  mem_pipe_reg[0] <= memreg;
                                           |end    
                                           |
                                           |always @ (posedge clk)
                                           |begin
                                           | for (i = 0; i < NBPIPE-1; i = i+1)
                                           |  if (mem_en_pipe_reg[i+1])
                                           |   mem_pipe_reg[i+1] <= mem_pipe_reg[i];
                                           |end      
                                           |
                                           |always @ (posedge clk)
                                           |begin
                                           | if (mem_en_pipe_reg[NBPIPE])
                                           |   dout <= mem_pipe_reg[NBPIPE-1];
                                           |end
                                           |						
                                           |	endmodule					""".stripMargin)
    case WRITE_FIRST => setInlineVerilog("""module SPURAM(
                                           |input clk,                    // Clock
                                           |input reset,
                                           |input we,                     // Write Enable
                                           |input mem_en,                 // Memory Enable
                                           |input[DWIDTH-1:0] din,       // Data Input  
                                           |input[$clog2(DEPTH)-1:0] addr,      // Address Input
                                           |output reg [DWIDTH-1:0] dout                // Data Output
                                           |
                                           |
                                           |);
                                           |
                                           |//  Xilinx UltraRAM Single Port Write First Mode.  This code implements 
                                           |//  a parameterizable UltraRAM block in write first mode. The behavior of this RAM is 
                                           |//  when data is written, the new memory contents at the write address are 
                                           |//  presented on the output port.
                                           |//
                                           |parameter DEPTH = 4096;  // Depth
                                           |parameter DWIDTH = 72;  // Data Width
                                           |parameter NBPIPE = 3;   // Number of pipeline Registers
                                           |
                                           |(* ram_style = "ultra" *)
                                           |reg [DWIDTH-1:0] mem[DEPTH-1:0];        // Memory Declaration
                                           |reg [DWIDTH-1:0] memreg;              
                                           |reg [DWIDTH-1:0] mem_pipe_reg[NBPIPE-1:0];    // Pipelines for memory
                                           |reg mem_en_pipe_reg[NBPIPE:0];                // Pipelines for memory enable  
                                           |
                                           |integer          i;
                                           |
                                           |// RAM : Both READ and WRITE have a latency of one
                                           |always @ (posedge clk)
                                           |begin
                                           | if(mem_en) 
                                           |  begin
                                           |   if(we)
                                           |    begin
                                           |     mem[addr] <= din;
                                           |     memreg <= din;
                                           |    end
                                           |   else
                                           |    memreg <= mem[addr];
                                           |  end
                                           |end
                                           |
                                           |// The enable of the RAM goes through a pipeline to produce a
                                           |// series of pipelined enable signals required to control the data
                                           |// pipeline.
                                           |always @ (posedge clk)
                                           |begin
                                           | mem_en_pipe_reg[0] <= mem_en;
                                           | for (i=0; i<NBPIPE; i=i+1)
                                           |   mem_en_pipe_reg[i+1] <= mem_en_pipe_reg[i];
                                           |end
                                           |
                                           |// RAM output data goes through a pipeline.
                                           |always @ (posedge clk)
                                           |begin
                                           | if (mem_en_pipe_reg[0])
                                           |  mem_pipe_reg[0] <= memreg;
                                           |end    
                                           |
                                           |always @ (posedge clk)
                                           |begin
                                           | for (i = 0; i < NBPIPE-1; i = i+1)
                                           |  if (mem_en_pipe_reg[i+1])
                                           |   mem_pipe_reg[i+1] <= mem_pipe_reg[i];
                                           |end      
                                           |
                                           |always @ (posedge clk)
                                           |begin
                                           | if (mem_en_pipe_reg[NBPIPE] )
                                           |   dout <= mem_pipe_reg[NBPIPE-1];
                                           |end
                                           |	
                                           |endmodule	""".stripMargin)
  }
}
