module pifo (
  input clk,
  input rst,

  input                pop_0;
  input  [BITPORT-1:0] oprt_0;
  output               ovld_0;
  output [BITPRIO-1:0] opri_0;
  output [BITDATA-1:0] odout_0;

  input                push_1;
  input  [BITPORT-1:0] uprt_1;
  input  [BITPRIO-1:0] upri_1;
  input  [BITDATA-1:0] udin_1;

  input                push_2;
  input  [BITPORT-1:0] uprt_2;
  input  [BITPRIO-1:0] upri_2;
  input  [BITDATA-1:0] udin_2;
);

parameter NUMPIFO = 1024;  
parameter BITPORT = 8; 
parameter BITPRIO = 16;
parameter BITDATA = 32;
parameter PIFO_ID = 0;

localparam BITPIFO = $clog2(NUMPIFO);
localparam FLOP_IDX  = 1;

reg  [BITPIFO+1:0] pifo_count;
wire [BITPIFO+1:0] new_pifo_count;

wire [1:0] shifter          [0:NUMPIFO-1];
wire [BITPORT-1:0] port_array [0:NUMPIFO-1];
wire [BITPRIO-1:0] prio_array [0:NUMPIFO-1];
wire [BITDATA-1:0] data_array [0:NUMPIFO-1];

always_ff @(posedge clk) begin
  if (rst) begin
    // reset logic
  end
  else begin
    // main logic
  end
end



endmodule