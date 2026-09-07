`timescale 1ns/1ps
// Isolated ordinary lookup; throughput one request/cycle, latency 8 cycles.
module PifoMesh (
  input wire clk, input wire reset,
  input wire write_valid, input wire [16:0] write_address,
  input wire [9:0] write_data,
  input wire read_valid, input wire [16:0] read_address,
  output wire response_valid, output wire [9:0] response_data
);
  reg [9:0] ram [0:131071];
  reg [9:0] data_pipe [0:7];
  reg [7:0] valid_pipe;
  initial begin
    $readmemb("lookup-zero.bin",ram);
  end
  always @(posedge clk) begin
    if (write_valid) ram[write_address] <= write_data;
    if (read_valid) data_pipe[0] <= ram[read_address];
    for (integer k=1; k<8; k=k+1) data_pipe[k] <= data_pipe[k-1];
    if (reset) valid_pipe <= 0;
    else begin
      valid_pipe[0] <= read_valid;
      for (integer k=1; k<8; k=k+1) valid_pipe[k] <= valid_pipe[k-1];
    end
  end
  assign response_valid = valid_pipe[7];
  assign response_data = data_pipe[7];
endmodule
