`timescale 1ns/1ps
// Compile against a 1-PE, 8-ID, 32-entry mesh. The runner defines DYNAMIC_CONFIG.
module configuration_contract_tb;
  reg clk=0, reset=1;
  reg pop_request=0, insert_request=0, control_valid=0;
  reg [2:0] command=0, control_port=0;
  reg [2:0] insert_flow=1, pop_port=1;
  reg [3:0] control_flow=0;
  reg [31:0] control_data=0;
  wire pop_ready, insert_ready, control_ready, commit_ready;
  wire result_valid;
  wire [0:0] result_engine;
  wire [2:0] result_flow;
  integer cycles=0, received=0, last_result=-1;
  PifoMesh dut (
    .clk(clk),.reset(reset),
    .io_dataRequest_valid(pop_request),.io_dataRequest_ready(pop_ready),
    .io_dataRequest_payload_engineId(1'b1),.io_dataRequest_payload_vPifoId(pop_port),
    .io_pop_valid(result_valid),.io_pop_ready(1'b1),
    .io_pop_payload_engineId(result_engine),.io_pop_payload_vPifoId(result_flow),
    .io_insert_0_valid(insert_request),.io_insert_0_ready(insert_ready),
    .io_insert_0_payload_engineId(1'b1),.io_insert_0_payload_vPifoId(insert_flow),
    .io_controlRequest_valid(control_valid),.io_controlRequest_ready(control_ready),
    .io_controlRequest_payload_command(command),.io_controlRequest_payload_engineId(1'b1),
    .io_controlRequest_payload_vPifoId(control_port),.io_controlRequest_payload_flowId(control_flow),
    .io_controlRequest_payload_data(control_data),.io_commitReady(commit_ready));

  task tick;
    #4;
    if (!reset && result_valid) begin
      if (result_engine !== 0) $fatal(1,"CONFIGURATION_FAIL output engine");
      received++; last_result=result_flow;
    end
    clk=1; #5; clk=0; #1;
    cycles++;
    if (cycles>4000) $fatal(1,"CONFIGURATION_FAIL watchdog");
    if (!reset && !`DYNAMIC_CONFIG && !commit_ready)
      $fatal(1,"CONFIGURATION_FAIL static commit unexpectedly stalls");
  endtask

  task control(input integer cmd, vp=0, flow=0, data=0);
    command=cmd; control_port=vp; control_flow=flow; control_data=data;
    control_valid=1; #1;
    while (!control_ready) tick();
    tick(); control_valid=0;
    repeat(8) tick();
  endtask

  task commit;
    control(3);
    while (!commit_ready) tick();
    repeat(8) tick();
  endtask

  task packet(input integer expected_flow);
    integer previous;
    previous=received;
    insert_request=1; #1;
    while (!insert_ready) tick();
    tick(); insert_request=0;
    repeat(20) tick();
    pop_request=1; #1;
    while (!pop_ready) tick();
    tick(); pop_request=0;
    repeat(40) tick();
    if (received!=previous+1 || last_result!=expected_flow)
      $fatal(1,"CONFIGURATION_FAIL expected one packet flow=%0d, received_delta=%0d actual_flow=%0d",
             expected_flow,received-previous,last_result);
  endtask

  initial begin
    repeat(4) tick(); reset=0; repeat(4) tick();
    // flow 1 -> PIFO 1; FIFO brain; token (engine=1, flow=1) -> terminal flow 1.
    control(0,1,0,1);
    control(4,1,0,3);
    control(1,1,9,1);
    commit();
    packet(1);

    // A normal table exposes this write immediately. An atomic table retains
    // the published result until the commit has traversed the same ingress.
    control(1,1,9,2);
    packet(`DYNAMIC_CONFIG ? 1 : 2);
    commit();
    packet(2);

    // Repeated commit messages must be harmless in the ordinary-table design.
    repeat(3) commit();
    packet(2);

    // Exercise the top address/port/token encoding too. The traffic simulator
    // reserves an empty-token ID, but the synthesized core uses explicit valid
    // bits and must transport the all-ones global-flow ID without losing it.
    control(0,7,0,7);
    control(4,7,0,3);
    control(1,7,15,7);
    commit();
    insert_flow=7; pop_port=7;
    packet(7);
    control(1,7,15,6);
    packet(`DYNAMIC_CONFIG ? 7 : 6);
    commit();
    packet(6);
    $display("CONFIGURATION_CONTRACT_PASS dynamic=%0d cycles=%0d packets=%0d",`DYNAMIC_CONFIG,cycles,received);
    $finish;
  end
endmodule
