package Chainsaw.memory

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._
import scala.language.postfixOps

case class TDPURAM(width: Int, depth: Int, pipeRegCount: Int = 4) extends BlackBox {
  val io = new Bundle {
    val clk:   Bool = in Bool () // Clock
    val reset: Bool = in Bool () // no use, just for simulation
// Port A
    val wea:     Bool = in Bool () // Write Enable
    val mem_ena: Bool = in Bool () // Memory Enable
    val dina:    Bits = in Bits (width bits) // Data Input
    val addra:   UInt = in UInt (log2Up(depth) bits) // Address Input
    val douta:   Bits = out Bits (width bits) // Data Output

// Port B
    val web:     Bool = in Bool () // Write Enable
    val mem_enb: Bool = in Bool () // Memory Enable
    val dinb:    Bits = in Bits (width bits) // Data Input
    val addrb:   UInt = in UInt (log2Up(depth) bits) // Address Input
    val doutb:   Bits = out Bits (width bits) // Data Output
  }

  noIoPrefix()
  mapClockDomain(clock = io.clk, reset = io.reset)

  addGenerics(("DEPTH", depth), ("DWIDTH", width), ("NBPIPE", pipeRegCount - 1))

  setInlineVerilog("""
  
//  Xilinx UltraRAM True Dual Port Mode.  This code implements 
//  a parameterizable UltraRAM block with write/read on both ports in 
//  No change behavior on both the ports . The behavior of this RAM is 
//  when data is written, the output of RAM is unchanged w.r.t each port. 
//  Only when write is inactive data corresponding to the address is 
//  presented on the output port.
//

module TDPURAM(
input clk,                   // Clock 
input reset,
// Port A
input wea,                     // Write Enable
input mem_ena,                // Memory Enable
input [DWIDTH-1:0] dina,       // Data Input 
input [$clog2(DEPTH)-1:0] addra,      // Address Input
output reg [DWIDTH-1:0] douta,                // Data Output

// Port B
input web,                     // Write Enable  
input mem_enb,                 // Memory Enable
input [DWIDTH-1:0] dinb,       // Data Input 
input [$clog2(DEPTH)-1:0] addrb,      // Address Input
output reg [DWIDTH-1:0] doutb  // Data Output

);



parameter DEPTH = 4096;  // Address Width
parameter DWIDTH = 72;  // Data Width
parameter NBPIPE = 3;   // Number of pipeline Registers



(* ram_style = "ultra" *)
reg [DWIDTH-1:0] mem[DEPTH-1:0];        // Memory Declaration

reg [DWIDTH-1:0] memrega;              
reg [DWIDTH-1:0] mem_pipe_rega[NBPIPE-1:0];    // Pipelines for memory
reg mem_en_pipe_rega[NBPIPE:0];                // Pipelines for memory enable  

reg [DWIDTH-1:0] memregb;              
reg [DWIDTH-1:0] mem_pipe_regb[NBPIPE-1:0];    // Pipelines for memory
reg mem_en_pipe_regb[NBPIPE:0];                // Pipelines for memory enable  
integer          i;

// RAM : Both READ and WRITE have a latency of one
always @ (posedge clk)
begin
 if(mem_ena) 
  begin
   if(wea)
    mem[addra] <= dina;
   else
    memrega <= mem[addra];
  end
end

// The enable of the RAM goes through a pipeline to produce a
// series of pipelined enable signals required to control the data
// pipeline.
always @ (posedge clk)
begin
 mem_en_pipe_rega[0] <= mem_ena;
 for (i=0; i < NBPIPE; i=i+1)
   mem_en_pipe_rega[i+1] <= mem_en_pipe_rega[i];
end

// RAM output data goes through a pipeline.
always @ (posedge clk)
begin
 if (mem_en_pipe_rega[0])
  mem_pipe_rega[0] <= memrega;
end    

always @ (posedge clk)
begin
 for (i = 0; i < NBPIPE-1; i = i+1)
  if (mem_en_pipe_rega[i+1])
   mem_pipe_rega[i+1] <= mem_pipe_rega[i];
end      

always @ (posedge clk)
begin
 if (mem_en_pipe_rega[NBPIPE])
   douta <= mem_pipe_rega[NBPIPE-1];
end


always @ (posedge clk)
begin
 if(mem_enb) 
  begin
   if(web)
    mem[addrb] <= dinb;
   else
    memregb <= mem[addrb];
  end
end

// The enable of the RAM goes through a pipeline to produce a
// series of pipelined enable signals required to control the data
// pipeline.
always @ (posedge clk)
begin
 mem_en_pipe_regb[0] <= mem_enb;
 for (i=0; i<NBPIPE; i=i+1)
   mem_en_pipe_regb[i+1] <= mem_en_pipe_regb[i];
end

// RAM output data goes through a pipeline.
always @ (posedge clk)
begin
 if (mem_en_pipe_regb[0])
  mem_pipe_regb[0] <= memregb;
end    

always @ (posedge clk)
begin
 for (i = 0; i < NBPIPE-1; i = i+1)
  if (mem_en_pipe_regb[i+1])
   mem_pipe_regb[i+1] <= mem_pipe_regb[i];
end      

always @ (posedge clk)
begin
 if (mem_en_pipe_regb[NBPIPE])
   doutb <= mem_pipe_regb[NBPIPE-1];
end

endmodule		""")
}

object TDPURAM {
  def apply(portA: SimpleDualPort, portB: SimpleDualPort): TDPURAM = {
    val ret = TDPURAM(portA.width, portA.depth)
    ret.io.mem_ena := portA.en
    ret.io.mem_enb := portB.en

    ret.io.wea := portA.we
    ret.io.web := portB.we

    ret.io.addra := portA.addr
    ret.io.addrb := portB.addr

    ret.io.dina := portA.wData
    ret.io.dinb := portB.wData

    portA.rData := ret.io.douta
    portB.rData := ret.io.doutb
    ret
  }
}
