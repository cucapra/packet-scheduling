`timescale 1ns/1ps
// The runner prepends dimension and module-name defines from the build manifest.
module pifo_contract_tb;
  localparam CAPACITY = `PIFO_CAPACITY;
  localparam PORT_BITS = `PIFO_PORT_BITS;
  localparam RANK_BITS = `PIFO_RANK_BITS;
  localparam DATA_BITS = `PIFO_DATA_BITS;
  reg clk=0, reset=1, push1=0, push2=0, pop=0;
  reg [PORT_BITS-1:0] port1=0, port2=0, pop_port=0;
  reg [RANK_BITS-1:0] rank1=0, rank2=0;
  reg [DATA_BITS-1:0] data1=0, data2=0;
  wire valid, exist, empty_port, drained;
  wire [PORT_BITS-1:0] response_port, drained_port;
  wire [RANK_BITS-1:0] response_rank;
  wire [DATA_BITS-1:0] response_data;
  `PIFO_MODULE dut (
    .clk(clk), .reset(reset),
    .io_push1_valid(push1), .io_push1_payload_port(port1),
    .io_push1_payload_priority(rank1), .io_push1_payload_data(data1),
    .io_push2_valid(push2), .io_push2_payload_port(port2),
    .io_push2_payload_priority(rank2), .io_push2_payload_data(data2),
    .io_popRequest_valid(pop), .io_popRequest_payload_port(pop_port),
    .io_popResponse_valid(valid), .io_popResponse_payload_exist(exist),
    .io_popResponse_payload_port(response_port),
    .io_popResponse_payload_priority(response_rank),
    .io_popResponse_payload_data(response_data),
    .io_popPortEmpty(empty_port), .io_portDrained_valid(drained),
    .io_portDrained_payload(drained_port));

  typedef struct packed {
    bit [PORT_BITS-1:0] port;
    bit [RANK_BITS-1:0] rank;
    bit [DATA_BITS-1:0] data;
  } entry_t;
  entry_t queue_model[$];
  int cycles=0;
  string phase="reset";
  int unsigned rng=32'h710fa123;

  // An independent stable-priority queue model, including overflow acceptance.
  task step(input bit pop_valid=0, input int requested_port=0,
            input bit push1_valid=0, input int p1=0, r1=0, d1=0,
            input bit push2_valid=0, input int p2=0, r2=0, d2=0);
    int selected;
    bit expected_exist, expected_drained, remaining;
    entry_t expected, added;
    clk=0;
    pop=pop_valid; pop_port=requested_port;
    push1=push1_valid; port1=p1; rank1=r1; data1=d1;
    push2=push2_valid; port2=p2; rank2=r2; data2=d2;
    #2;
    selected=-1;
    foreach (queue_model[i]) begin
      if (queue_model[i].port == pop_port) begin
        if (selected < 0) selected=i;
        else if (queue_model[i].rank < queue_model[selected].rank) selected=i;
      end
    end
    if (empty_port !== (selected < 0))
      $fatal(1,"CONTRACT_FAIL phase=%s cycle=%0d empty signal",phase,cycles);
    expected_exist=pop_valid && selected >= 0;
    if (expected_exist) begin
      expected=queue_model[selected];
      queue_model.delete(selected);
    end
    if (push1_valid && queue_model.size() < CAPACITY) begin
      added.port=port1; added.rank=rank1; added.data=data1;
      queue_model.push_back(added);
    end
    if (push2_valid && queue_model.size() < CAPACITY) begin
      added.port=port2; added.rank=rank2; added.data=data2;
      queue_model.push_back(added);
    end
    remaining=0;
    foreach (queue_model[i]) if (queue_model[i].port == pop_port) remaining=1;
    expected_drained=expected_exist && !remaining;
    #3; clk=1; #1;
    if (valid !== pop_valid || (pop_valid && (exist !== expected_exist || response_port !== pop_port)))
      $fatal(1,"CONTRACT_FAIL phase=%s cycle=%0d response valid/exist/port",phase,cycles);
    if (expected_exist && (response_data !== expected.data || response_rank !== expected.rank))
      $fatal(1,"CONTRACT_FAIL phase=%s cycle=%0d requested_port=%0d expected_rank=%0d expected_data=%0d actual_rank=%0d actual_data=%0d",
             phase,cycles,pop_port,expected.rank,expected.data,response_rank,response_data);
    if (drained !== expected_drained || (expected_drained && drained_port !== pop_port))
      $fatal(1,"CONTRACT_FAIL phase=%s cycle=%0d drain signal",phase,cycles);
    #4; clk=0; cycles++;
  endtask

  task clear_core;
    reset=1; push1=0; push2=0; pop=0;
    queue_model.delete();
    repeat(3) begin #5; clk=1; #5; clk=0; end
    reset=0;
    step();
  endtask

  function int unsigned random_word();
    rng=rng ^ (rng << 13);
    rng=rng ^ (rng >> 17);
    rng=rng ^ (rng << 5);
    return rng;
  endfunction

  initial begin
    phase="empty and immediate forwarding";
    clear_core();
    step(1,1);
    step(1,1,1,1,5,55);
    step(1,1);
    step(1,1);
    $display("CONTRACT_PHASE_PASS %s",phase);

    phase="interleaved ports, consecutive pops";
    clear_core();
    step(0,0,1,1,1,11);
    step(0,0,1,2,2,22);
    step(0,0,1,1,3,33);
    step(); step();
    step(1,1);
    step(1,1);
    step(1,2);
    $display("CONTRACT_PHASE_PASS %s",phase);

    phase="ties and simultaneous pop/two pushes";
    clear_core();
    step(0,0,1,1,4,10,1,1,4,11);
    step(1,1,1,1,4,12,1,2,1,13);
    step(1,1); step(1,1); step(1,2);
    $display("CONTRACT_PHASE_PASS %s",phase);

    phase="full capacity, overflow, full pop/push";
    clear_core();
    for(int i=0;i<CAPACITY;i++) step(0,0,1,1,i % 128,i);
    step(0,0,1,1,0,99,1,1,0,100);
    step(1,1,1,1,255,101,1,1,0,102);
    for(int i=0;i<CAPACITY;i++) step(1,1);
    step(1,1);
    $display("CONTRACT_PHASE_PASS %s",phase);

    phase="deterministic random traffic";
    clear_core();
    repeat(2000) begin
      automatic int unsigned a=random_word(), b=random_word(), c=random_word();
      step(a[0],a[8:4],a[1],b[4:0],b[15:8],b[23:16],a[2],c[4:0],c[15:8],c[23:16]);
    end
    for(int p=0;p<(1<<PORT_BITS);p++) begin
      repeat(CAPACITY+1) step(1,p);
    end
    $display("CONTRACT_PHASE_PASS %s",phase);
    $display("PIFO_CONTRACT_PASS cycles=%0d capacity=%0d",cycles,CAPACITY);
    $finish;
  end
endmodule
