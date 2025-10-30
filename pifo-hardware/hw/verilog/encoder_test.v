module test_encoder();

// Test parameters
parameter WIDTH = 1024;
parameter LOG_WIDTH = 10;
parameter CLK_PERIOD = 10; // 10ns clock period

// Signals
reg clk;
reg rst;
reg [WIDTH-1:0] decode;
wire [LOG_WIDTH-1:0] encode;
wire valid;

// Instantiate the module under test
priority_encode_log #(
  .width(WIDTH),
  .log_width(LOG_WIDTH)
) dut (
  .clk(clk),
  .rst(rst),
  .decode(decode),
  .encode(encode),
  .valid(valid)
);

// Clock generation
initial begin
  clk = 0;
  forever #(CLK_PERIOD/2) clk = ~clk;
end

// Reset generation
initial begin
  rst = 1;
  #(CLK_PERIOD * 2);
  rst = 0;
end

// Test stimulus
initial begin
  // VCD dump for waveform viewing
  $dumpfile("priority_encode_log.vcd");
  $dumpvars(0, test_encoder);
  
  decode = 0;
  
  // Test 1: All zeros
  #(CLK_PERIOD);
  $display("Test 1: All zeros");
  $display("  decode = %b", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(valid == 0) else $error("Expected valid=0 for all-zero input");
  
  // Test 2: Single bit at position 0
  #(CLK_PERIOD);
  decode = 1'b1;
  #(CLK_PERIOD);
  $display("\nTest 2: Single bit at position 0");
  $display("  decode = %b (bit 0)", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 0 && valid == 1) else $error("Expected encode=0, valid=1");
  
  // Test 3: Single bit at position 5
  #(CLK_PERIOD);
  decode = 32'h20;  // bit 5
  #(CLK_PERIOD);
  $display("\nTest 3: Single bit at position 5");
  $display("  decode = %b (bit 5)", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 5 && valid == 1) else $error("Expected encode=5, valid=1");
  
  // Test 4: Single bit at position 10
  #(CLK_PERIOD);
  decode = (1 << 10);
  #(CLK_PERIOD);
  $display("\nTest 4: Single bit at position 10");
  $display("  decode bit 10");
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 10 && valid == 1) else $error("Expected encode=10, valid=1");
  
  // Test 5: Multiple bits - should select the lowest
  #(CLK_PERIOD);
  decode = 32'hFC;  // bits 2-7 set
  #(CLK_PERIOD);
  $display("\nTest 5: Multiple bits (0b11111100)");
  $display("  decode = %b", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 2 && valid == 1) else $error("Expected encode=2 (lowest bit), valid=1");
  
  // Test 6: Multiple bits starting from position 5
  #(CLK_PERIOD);
  decode = 32'h3E0;  // bits 5-9 set
  #(CLK_PERIOD);
  $display("\nTest 6: Multiple bits (bits 5-9)");
  $display("  decode = %b", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 5 && valid == 1) else $error("Expected encode=5 (lowest bit), valid=1");
  
  // Test 7: Alternating pattern
  #(CLK_PERIOD);
  decode = 32'hAAAAAAAA;  // bits 1,3,5,7,... set
  #(CLK_PERIOD);
  $display("\nTest 7: Alternating pattern (0xAAAAAAAA)");
  $display("  decode = %b", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 1 && valid == 1) else $error("Expected encode=1 (lowest bit), valid=1");
  
  // Test 8: All ones (within 32-bit range for this test)
  #(CLK_PERIOD);
  decode = 32'hFFFFFFFF;
  #(CLK_PERIOD);
  $display("\nTest 8: All ones (32-bit)");
  $display("  decode = %b", decode);
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 0 && valid == 1) else $error("Expected encode=0 (lowest bit), valid=1");
  
  // Test 9: Large position - bit 512
  #(CLK_PERIOD);
  decode = 0;
  decode[512] = 1'b1;
  #(CLK_PERIOD);
  $display("\nTest 9: Single bit at position 512");
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 512 && valid == 1) else $error("Expected encode=512, valid=1");
  
  // Test 10: Multiple bits with highest priority at 1023
  #(CLK_PERIOD);
  decode = 0;
  decode[1023] = 1'b1;
  decode[512] = 1'b1;
  decode[100] = 1'b1;
  #(CLK_PERIOD);
  $display("\nTest 10: Multiple bits (100, 512, 1023) - should select 100");
  $display("  encode = %d, valid = %b", encode, valid);
  assert(encode == 100 && valid == 1) else $error("Expected encode=100 (lowest bit), valid=1");
  
  #(CLK_PERIOD * 2);
  $display("\n✓ All tests passed!");
  $finish;
end

endmodule