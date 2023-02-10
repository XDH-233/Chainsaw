package Chainsaw.memory
import spinal.core._

import scala.language.postfixOps

case class SDPURAM(width: Int, depth: Int, readLatency: Int = 4) extends BlackBox {
  val io = new Bundle {
    val clk:    Bool = in Bool ()
    val reset:  Bool = in Bool ()
    val wea:    Bool = in Bool ()
    val mem_en: Bool = in Bool ()
    val dina:   Bits = in Bits (width bits)
    val addra:  UInt = in UInt (log2Up(depth) bits)
    val addrb:  UInt = in UInt (log2Up(depth) bits)
    val doutb:  Bits = out Bits (width bits)
  }
  noIoPrefix()
  addGenerics(("DEPTH", depth), ("DWIDTH", width), ("NBPIPE", readLatency - 1))
  mapClockDomain(clock = io.clk, reset = io.reset)

  def write(addr: UInt, data: Bits, en: Bool): Unit = {
    io.wea   := en
    io.addra := addr
    io.dina  := data
  }

  def read(addr: UInt): Bits = {
    io.addrb := addr
    io.doutb
  }

  setInlineVerilog("""
                     |//  Xilinx UltraRAM Simple Dual Port.  This code implements 
                     |//  a parameterizable UltraRAM block with 1 Read and 1 write
                     |//  when addra == addrb, old data will show at doutb 
                     |
                     |
                     |module SDPURAM(
                     |    input clk,                    // Clock 
                     |    input reset,
                     |    input wea,                    // Write Enable
                     |    input mem_en,                 // Memory Enable
                     |    input [DWIDTH-1:0] dina,      // Data <wire_or_reg>  
                     |    input [$clog2(DEPTH)-1:0] addra,     // Write Address
                     |    input [$clog2(DEPTH)-1:0] addrb,     // Read  Address
                     |    output reg [DWIDTH-1:0] doutb // Data Output
                     |);
                     |
                     |
                     |
                     |parameter DEPTH = 4096;  
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
                     |   if(wea)
                     |     mem[addra] <= dina;
                     |
                     |   memreg <= mem[addrb];
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
                     |   doutb <= mem_pipe_reg[NBPIPE-1];
                     |end
                     |
                     |					
                     |endmodule""".stripMargin)

}
