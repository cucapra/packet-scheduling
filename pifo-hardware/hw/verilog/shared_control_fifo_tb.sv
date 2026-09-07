`timescale 1ns/1ps
`ifndef CONTROL_DEPTH
`define CONTROL_DEPTH 4
`endif
// The reference model expands each accepted epoch into its first execution
// followed by only its mapper writes. It does not inspect DUT RAM or pointers.
module shared_control_fifo_tb;
  localparam D=`CONTROL_DEPTH;
  reg clk=0, reset=1, valid=0, out_ready=1;
  reg [2:0] command=0, port=0;
  reg [1:0] engine=1;
  reg [4:0] flow=0;
  reg [31:0] data=0;
  wire ready, out_valid, busy;
  wire [2:0] out_command, out_port;
  wire [1:0] out_engine;
  wire [4:0] out_flow;
  wire [31:0] out_data;
  wire [$clog2(D+1)-1:0] available;
  wire [44:0] input_word={command,engine,port,flow,data};
  wire [44:0] output_word={out_command,out_engine,out_port,out_flow,out_data};
  reg [44:0] expected [0:196607 + 4*D], epoch [0:D-1], held;
  reg expected_replay [0:196607 + 4*D];
  reg stalled=0;
  reg [31:0] random_state=32'h719af02d;
  integer expected_write=0, expected_read=0, epoch_count=0;
  integer cycles=0, inputs=0, outputs=0, replays=0, commits=0;
  integer stalls=0, replay_stalls=0, simultaneous=0, ready_mode=1;
  reg pushed;

  ReplayControlFifo dut (
    .clk(clk),.reset(reset),.io_push_valid(valid),.io_push_ready(ready),
    .io_push_payload_command(command),.io_push_payload_engineId(engine),
    .io_push_payload_vPifoId(port),.io_push_payload_flowId(flow),.io_push_payload_data(data),
    .io_pop_valid(out_valid),.io_pop_ready(out_ready),
    .io_pop_payload_command(out_command),.io_pop_payload_engineId(out_engine),
    .io_pop_payload_vPifoId(out_port),.io_pop_payload_flowId(out_flow),.io_pop_payload_data(out_data),
    .io_replaying(busy),.io_available(available));

  task step;
    random_state=random_state ^ (random_state<<13);
    random_state=random_state ^ (random_state>>17);
    random_state=random_state ^ (random_state<<5);
    out_ready=ready_mode<0 ? random_state[1:0]!=0 : ready_mode!=0;
    #3;
    pushed=valid && ready;
    if (!reset) begin
      if (available>D-1) $fatal(1,"FIFO_FAIL: invalid credits");
      if (busy && ready) $fatal(1,"FIFO_FAIL: ingress accepted during replay");
      if (stalled && (!out_valid || output_word!==held))
        $fatal(1,"FIFO_FAIL: output changed under backpressure");
      if (pushed) begin
        expected[expected_write]=input_word; expected_replay[expected_write]=0; expected_write++;
        inputs++;
        if (command==0 || command==1) begin epoch[epoch_count]=input_word;epoch_count++;end
        if (command==3) begin
          for (integer k=0;k<epoch_count;k=k+1) begin
            expected[expected_write]=epoch[k];expected_replay[expected_write]=1;expected_write++;
          end
          epoch_count=0;
        end
      end
      if (out_valid && out_ready) begin
        if (expected_read>=expected_write || output_word!==expected[expected_read] ||
            busy!==expected_replay[expected_read])
          $fatal(1,"FIFO_FAIL: order/data/phase at item %0d: got %h replay=%b expected %h replay=%b",expected_read,output_word,busy,expected[expected_read],expected_replay[expected_read]);
        expected_read++;outputs++;
        if (busy) replays++;
        if (out_command==3) begin
          if (busy) $fatal(1,"FIFO_FAIL: replayed commit");
          commits++;
        end
        if (pushed) simultaneous++;
      end
      stalled=out_valid && !out_ready;
      if (stalled) begin held=output_word;stalls++;if(busy) replay_stalls++;end
    end else begin
      expected_write=0;expected_read=0;epoch_count=0;stalled=0;
    end
    clk=1;#2;clk=0;#5;cycles++;
    if(cycles>200000) $fatal(1,"FIFO_FAIL: watchdog");
  endtask

  task send(input integer op, value=0);
    command=op;engine=1+(value&1);port=value;flow=value>>1;
    data=32'hf0a00000 ^ value;valid=1;
    step();while(!pushed) step();valid=0;
  endtask

  task drain;
    valid=0;
    while(expected_read<expected_write || busy || available!=D-1) step();
    repeat(5) step();
    if(out_valid) $fatal(1,"FIFO_FAIL: extra output after drain");
  endtask

  initial begin
    repeat(4) step();reset=0;repeat(3) step();
    send(3);send(3);drain(); // Empty and consecutive commits.
    // Delay this commit to overlap its ingress with the mapper's execution,
    // including the minimum depth where ordinary traffic cannot overlap.
    send(0,7);step();send(3);drain();
    // Immediate commands without mapper updates must not accumulate forever.
    for(integer k=0;k<64;k=k+1) send(k%5==0 ? 7 : (k%4==0 ? 2 : 4+(k%3)),k);
    drain();
    // A fully retained epoch still has a slot for commit, even with a stalled consumer.
    ready_mode=0;
    for(integer k=0;k<D-1;k=k+1) send(k%2,k);
    command=4;#1;if(ready || available!=0) $fatal(1,"FIFO_FAIL: reserved slot exposed to non-commit");
    command=3;#1;if(!ready) $fatal(1,"FIFO_FAIL: no room for commit at full epoch");
    send(3);repeat(13) step();
    if(ready) $fatal(1,"FIFO_FAIL: physical full accepted another commit");
    ready_mode=-1;drain();
    // Three deterministic random streams, arbitrary output backpressure, all
    // command kinds, full-width payloads, mixed epochs, and many ring wraps.
    for(integer seed=1;seed<=3;seed=seed+1) begin
      random_state=32'h6d2b79f5 ^ (seed*32'h934c12a7);
      for(integer batch=0;batch<180;batch=batch+1) begin
        for(integer k=0;k<batch%D;k=k+1) begin
          if(k%3==0) send(batch%2,batch*31+k);
          else send(batch%5==0 ? 7 : (batch%4==0 ? 2 : 4+(k%3)),batch*17+k);
        end
        send(3);
        if(batch%7==0) send(3);
      end
      drain();
    end
    // Reset flushes retained/in-flight controller commands, without reading stale RAM.
    ready_mode=1;send(0,19);send(3);
    while(!busy) step();ready_mode=0;repeat(3) step();
    reset=1;repeat(3) step();reset=0;ready_mode=1;repeat(3) step();
    if(out_valid || busy || available!=D-1) $fatal(1,"FIFO_FAIL: reset did not flush queue");
    send(6,32'h7fffff);send(3);drain();
    if(stalls==0 || replay_stalls==0 || simultaneous==0 || inputs<80*D)
      $fatal(1,"FIFO_FAIL: insufficient concurrency/wrap coverage");
    $display("SHARED_FIFO_PASS depth=%0d cycles=%0d accepted=%0d outputs=%0d replay_writes=%0d commits=%0d stalls=%0d replay_stalls=%0d simultaneous=%0d seeds=3",D,cycles,inputs,outputs,replays,commits,stalls,replay_stalls,simultaneous);
    $finish;
  end
endmodule
