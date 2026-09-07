module memory_probe(input wire clk, input wire we, input wire [22:0] wa, ra, input wire [31:0] din, output reg [31:0] dout);
(* ram_style = "block" *) reg [31:0] mem[0:8388607];
always @(posedge clk) begin
  if (we) mem[wa] <= din;
  dout <= mem[ra];
end
endmodule
