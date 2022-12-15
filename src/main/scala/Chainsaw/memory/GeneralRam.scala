package Chainsaw.memory

import Chainsaw._
import Chainsaw.xilinx._
import spinal.core._

import scala.language.postfixOps

// TODO: a unified test for RAMs

/** 1W1R RAM with arbitrary width and depth
  */
case class SingleRam(width: Int, depth: Int, readLatency: Int, w2rLatency: Int) extends Component {

  val writeLatency = w2rLatency - readLatency

  def estimation = {
    VivadoUtilEstimation(width.divideAndCeil(72) * depth.divideAndCeil(1 << 12))
  }

  val readAddr = in UInt (log2Up(depth) bits)
  val readData = out Bits (width bits)

  val writeAddr = in UInt (log2Up(depth) bits)
  val writeData = in Bits (width bits)
  val writeEn   = in Bool ()

  val mem = Mem(Bits(width bits), depth)
  mem.setAsUltraRam()
  readData := mem.readSync(readAddr).d(readLatency - 1)
  mem.write(writeAddr.d(writeLatency), writeData.d(writeLatency), writeEn.d(writeLatency))
}

/** dual-port RAM with arbitrary width and depth
  */
case class DoubleRam(width: Int, depth: Int, readLatency: Int, w2rLatency: Int) extends Component {

  val writeLatency = w2rLatency - readLatency

  def estimation = {
    VivadoUtilEstimation(width.divideAndCeil(72) * depth.divideAndCeil(1 << 12))
  }

  val readAddr = in Vec (UInt(log2Up(depth) bits), 2)
  val readData = out Vec (Bits(width bits), 2)

  val writeAddr = in Vec (UInt(log2Up(depth) bits), 2)
  val writeData = in Vec (Bits(width bits), 2)
  val writeEn   = in Vec (Bool(), 2)

  val mem = Mem(Bits(width bits), depth)
  mem.setAsBlockRam()
  readData.zip(readAddr).foreach { case (data, addr) => data := mem.readSync(addr).d(readLatency - 1) }
  writeAddr.indices.foreach(i => mem.write(writeAddr(i).d(writeLatency), writeData(i).d(writeLatency), writeEn(i).d(writeLatency)))
}

/** multi-ported RAM based on SingleRAM/DoubleRAM
  */
case class GeneralRam(width: Int, depth: Int, readPort: Int, writePort: Int, readLatency: Int, w2rLatency: Int) extends Component {}

case class tdp_uram(width: Int, depth: Int, readLatency: Int) extends BlackBox {
  val io = new Bundle {
    val clk = in Bool () // Clock
// Port A
    val wea     = in Bool () // Write Enable
    val mem_ena = in Bool () // Memory Enable
    val dina    = in Bits (width bits) // Data Input
    val addra   = in UInt (log2Up(depth) bits) // Address Input
    val douta   = out Bits (width bits) // Data Output

// Port B
    val web     = in Bool () // Write Enable
    val mem_enb = in Bool () // Memory Enable
    val dinb    = in Bits (width bits) // Data Input
    val addrb   = in UInt (log2Up(depth) bits) // Address Input
    val doutb   = out Bits (width bits) // Data Output
  }

  noIoPrefix()
  mapClockDomain(clock = io.clk)

  addGenerics(("DEPTH", depth), ("DWIDTH", width), ("NBPIPE", readLatency - 1))

  setInlineVerilog("""
  
//  Xilinx UltraRAM True Dual Port Mode.  This code implements 
//  a parameterizable UltraRAM block with write/read on both ports in 
//  No change behavior on both the ports . The behavior of this RAM is 
//  when data is written, the output of RAM is unchanged w.r.t each port. 
//  Only when write is inactive data corresponding to the address is 
//  presented on the output port.
//

module tdp_uram(
input clk,                   // Clock 
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
