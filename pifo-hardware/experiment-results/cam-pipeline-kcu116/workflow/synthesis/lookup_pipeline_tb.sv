`timescale 1ns/1ps
module lookup_pipeline_tb;
  localparam A=`LOOKUP_ADDRESS_BITS, D=`LOOKUP_DATA_BITS;
  localparam SHORT=`LOOKUP_SHORT_LATENCY, LONG=`LOOKUP_LONG_LATENCY;
  reg clk=0;
  always #5 clk=~clk;
  reg reset=1, write_valid=0, read_valid=0;
  reg [A-1:0] write_address=0, read_address=0;
  reg [D-1:0] write_data=0;
  wire short_valid, long_valid;
  wire [D-1:0] short_data, long_data;
  LookupShort short_lookup(.clk(clk),.reset(reset),.write_valid(write_valid),
    .write_address(write_address),.write_data(write_data),.read_valid(read_valid),
    .read_address(read_address),.response_valid(short_valid),.response_data(short_data));
  LookupLong long_lookup(.clk(clk),.reset(reset),.write_valid(write_valid),
    .write_address(write_address),.write_data(write_data),.read_valid(read_valid),
    .read_address(read_address),.response_valid(long_valid),.response_data(long_data));
  reg [D-1:0] model [0:(1<<A)-1];
  reg [LONG-1:0] expected_valid=0;
  reg [D-1:0] expected_data [0:LONG-1];
  integer checked_short=0, checked_long=0;
  reg [31:0] rng=32'h93a451ce;

  always @(posedge clk) begin
    if (reset) expected_valid=0;
    else begin
      for (integer k=LONG-1;k>0;k=k-1) begin
        expected_valid[k]=expected_valid[k-1];
        expected_data[k]=expected_data[k-1];
      end
      expected_valid[0]=read_valid;
      expected_data[0]=model[read_address];
    end
    // Read-first behavior on a same-cycle read/write collision.
    if (write_valid) model[write_address]=write_data;
    #1;
    if (short_valid !== expected_valid[SHORT-1] || long_valid !== expected_valid[LONG-1])
      $fatal(1,"LOOKUP_PIPELINE_FAIL: valid alignment");
    if (short_valid) begin
      if (short_data !== expected_data[SHORT-1]) $fatal(1,"LOOKUP_PIPELINE_FAIL: short data");
      checked_short=checked_short+1;
    end
    if (long_valid) begin
      if (long_data !== expected_data[LONG-1]) $fatal(1,"LOOKUP_PIPELINE_FAIL: long data");
      checked_long=checked_long+1;
    end
  end

  initial begin
    for (integer k=0;k<(1<<A);k=k+1) model[k]=0;
    repeat(3) @(negedge clk);
    reset=0;
    // Bubbles, bursts, same-address collisions, highest address, and reset
    // during outstanding reads. Revisit a working set to observe written data.
    for (integer n=0;n<320;n=n+1) begin
      rng={rng[30:0],rng[31]^rng[21]^rng[1]^rng[0]};
      write_valid=(n%3!=0); read_valid=(n%5!=0);
      write_address=(n%7==0)?{A{1'b1}}:rng[5:0];
      read_address=(n%4==0)?write_address:((n%9==0)?{A{1'b1}}:rng[11:6]);
      write_data=rng[D-1:0];
      reset=(n==150 || n==151);
      @(negedge clk);
    end
    reset=0;write_valid=0;read_valid=0;
    repeat(LONG+2) @(negedge clk);
    if (checked_short<200 || checked_long<200) $fatal(1,"LOOKUP_PIPELINE_FAIL: insufficient responses");
    $display("LOOKUP_PIPELINE_PASS short_latency=%0d long_latency=%0d checked_short=%0d checked_long=%0d",
      SHORT,LONG,checked_short,checked_long);
    $finish;
  end
endmodule
