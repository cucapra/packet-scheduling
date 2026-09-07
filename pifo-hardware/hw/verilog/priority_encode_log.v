module priority_encode_log (
  clk,rst,
  decode,
  encode,valid
);

parameter width = 1024;
parameter log_width = 10;

localparam pot_width = 1 << log_width;

input                  clk;
input                  rst;
input  [width-1:0]     decode;
output [log_width-1:0] encode;
output                 valid;

wire [pot_width-1:0] pot_decode = {pot_width{1'b0}} | decode;

// A balanced tree of continuous assignments. Each net has exactly one driver;
// no always_comb blocks read/write different slices of one shared array.
wire [2*pot_width-1:1] present;
wire [log_width-1:0] position [1:2*pot_width-1];
genvar i;
generate
  for (i=0; i<pot_width; i=i+1) begin: leaves
    localparam [log_width-1:0] leaf_index = i;
    assign present[pot_width+i] = pot_decode[i];
    assign position[pot_width+i] = leaf_index;
  end
  for (i=1; i<pot_width; i=i+1) begin: branches
    assign present[i] = present[2*i] | present[2*i+1];
    assign position[i] = present[2*i] ? position[2*i] : position[2*i+1];
  end
endgenerate

assign valid = present[1];
assign encode = position[1];

endmodule // encoder_test
