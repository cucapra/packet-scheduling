`timescale 1ns/1ps
// Two PEs, eight IDs, external PIFO boundary, four replay-log entries.
module replay_controller_tb;
  reg clk=0, reset=1, valid=0;
  reg [2:0] command=0, port=0;
  reg [1:0] engine=1;
  reg [4:0] flow=0;
  reg [31:0] data=0;
  wire ready, commit_ready, replay_busy;
  wire [2:0] available;
  integer cycles=0, swaps=0, busy_cycles=0, accepted_updates=0;
  integer checked_reads=0, reads_during_replay=0, reads_on_swap=0;
  reg probe_valid=0;
  reg [2:0] expected_pre [0:1][0:7];
  reg [4:0] expected_post [0:1][0:255];
  reg [4:0] visible_post [0:255];

  PifoMesh dut (
    .clk(clk),.reset(reset),
    .io_controlRequest_valid(valid),.io_controlRequest_ready(ready),
    .io_controlRequest_payload_command(command),.io_controlRequest_payload_engineId(engine),
    .io_controlRequest_payload_vPifoId(port),.io_controlRequest_payload_flowId(flow),
    .io_controlRequest_payload_data(data),.io_commitReady(commit_ready),
    .io_replayBusy(replay_busy),.io_replayLogAvailable(available),
    .io_dataRequest_valid(1'b0),.io_dataRequest_payload_engineId(2'b0),.io_dataRequest_payload_vPifoId(3'b0),
    .io_pop_ready(1'b1),
    .io_insert_0_valid(1'b0),.io_insert_0_payload_engineId(2'b0),.io_insert_0_payload_vPifoId(3'b0),
    .io_insert_1_valid(1'b0),.io_insert_1_payload_engineId(2'b0),.io_insert_1_payload_vPifoId(3'b0),
    .io_pifo_0_popResponse_valid(probe_valid),.io_pifo_0_popResponse_payload_exist(1'b1),
    .io_pifo_0_popResponse_payload_priority(8'b0),.io_pifo_0_popResponse_payload_data(5'd31),
    .io_pifo_0_popResponse_payload_port(3'd7),.io_pifo_0_popPortEmpty(1'b0),
    .io_pifo_0_portDrained_valid(1'b0),.io_pifo_0_portDrained_payload(3'b0),
    .io_pifo_1_popResponse_valid(1'b0),.io_pifo_1_popResponse_payload_exist(1'b0),
    .io_pifo_1_popResponse_payload_priority(8'b0),.io_pifo_1_popResponse_payload_data(5'b0),
    .io_pifo_1_popResponse_payload_port(3'b0),.io_pifo_1_popPortEmpty(1'b1),
    .io_pifo_1_portDrained_valid(1'b0),.io_pifo_1_portDrained_payload(3'b0));

  task tick;
    reg before_busy, before_bank, had_read;
    reg [4:0] expected_read;
    probe_valid=!reset && cycles%3==0;
    #3;
    before_busy=replay_busy;
    before_bank=dut.pifoEngines_0.enque_enqueMapper.activeBank;
    had_read=dut.pifoEngines_0.deque_dequeMapper.io_readReq_valid;
    expected_read=visible_post[dut.pifoEngines_0.deque_dequeMapper.io_readReq_payload];
    clk=1; #2;
    if (!reset) begin
      if (dut.pifoEngines_0.enque_enqueMapper.activeBank !== dut.pifoEngines_1.enque_enqueMapper.activeBank ||
          dut.pifoEngines_0.enque_enqueMapper.activeBank !== dut.pifoEngines_0.deque_dequeMapper.activeBank ||
          dut.pifoEngines_0.enque_enqueMapper.activeBank !== dut.pifoEngines_1.deque_dequeMapper.activeBank)
        $fatal(1,"REPLAY_FAIL: non-atomic global bank swap");
      if (before_bank !== dut.pifoEngines_0.enque_enqueMapper.activeBank) begin
        swaps++;
        if (before_busy) $fatal(1,"REPLAY_FAIL: commit executed during replay");
        if (had_read) reads_on_swap++;
        for (integer k=0;k<256;k=k+1) visible_post[k]=expected_post[0][k];
      end
      if (had_read) begin
        checked_reads++;
        if (before_busy) reads_during_replay++;
        if (!dut.pifoEngines_0.deque_dequeMapper.io_readRes_valid ||
            dut.pifoEngines_0.deque_dequeMapper.io_readRes_payload !== expected_read)
          $fatal(1,"REPLAY_FAIL: lookup changed epoch across commit/replay");
      end
      if (replay_busy) begin
        busy_cycles++;
        if (commit_ready || ready) $fatal(1,"REPLAY_FAIL: configuration accepted during replay");
      end
      if (available>4) $fatal(1,"REPLAY_FAIL: log credit underflow");
    end
    clk=0; #5; cycles++;
    if (cycles>12000) $fatal(1,"REPLAY_FAIL: watchdog");
  endtask

  task send(input integer op, pe=1, vp=0, token=0, value=0);
    command=op;engine=pe;port=vp;flow=token;data=value;valid=1;#1;
    while (!ready) tick();
    if (op==0) begin expected_pre[pe-1][vp]=value;accepted_updates++;end
    if (op==1) begin expected_post[pe-1][(vp<<5)|token]=value;accepted_updates++;end
    tick();valid=0;#1;
  endtask

`define CHECK_PE(N) \
    for (integer k=0;k<8;k=k+1) begin \
      if (dut.pifoEngines_``N``.enque_enqueMapper.banks_0[k] !== expected_pre[N][k] || \
          dut.pifoEngines_``N``.enque_enqueMapper.banks_1[k] !== expected_pre[N][k]) \
        $fatal(1,"REPLAY_FAIL: pre banks differ from update history PE=%0d address=%0d",N,k); \
    end \
    for (integer k=0;k<256;k=k+1) begin \
      if (dut.pifoEngines_``N``.deque_dequeMapper.banks_0[k] !== expected_post[N][k] || \
          dut.pifoEngines_``N``.deque_dequeMapper.banks_1[k] !== expected_post[N][k]) \
        $fatal(1,"REPLAY_FAIL: post banks differ from update history PE=%0d address=%0d",N,k); \
    end

  task synchronized_to(input integer count);
    while (swaps<count || replay_busy || available!=4) tick();
    repeat(2) tick();
    if (swaps!=count) $fatal(1,"REPLAY_FAIL: unexpected commit count");
    `CHECK_PE(0)
    `CHECK_PE(1)
  endtask

  initial begin
    for (integer p=0;p<2;p=p+1) begin
      for (integer k=0;k<8;k=k+1) expected_pre[p][k]=0;
      for (integer k=0;k<256;k=k+1) expected_post[p][k]=0;
    end
    for (integer k=0;k<256;k=k+1) visible_post[k]=0;
    repeat(4) tick();reset=0;repeat(4) tick();
    send(3); synchronized_to(1); // Empty commit.
    send(0,1,7,0,32'hdeadbeef);
    send(1,1,7,31,5);
    send(1,1,7,31,6); // Duplicate address: last write wins after replay too.
    send(0,2,7,0,3);
    if (available!==0) $fatal(1,"REPLAY_FAIL: missing ingress reservations");
    repeat(20) tick();
    command=0;#1;
    if (ready) $fatal(1,"REPLAY_FAIL: full log permits another mapper update");
    // An unbanked command and a commit remain legal at full log capacity.
    send(4,2,7,0,3);
    if (available!==0) $fatal(1,"REPLAY_FAIL: unbanked command consumed log credit");
    send(3);send(3);synchronized_to(3);
    // Partial epochs preserve all earlier untouched entries; zero-update epochs
    // and alternating engines exercise empty FIFO and pointer wraparound.
    for (integer epoch=0;epoch<24;epoch=epoch+1) begin
      for (integer k=0;k<(epoch%5);k=k+1) begin
        if (k%2==0) send(0,1+(epoch%2),(epoch+k)%8,0,epoch+k);
        else if (epoch%2==0) send(1,1,7,31,(epoch+k)%8);
        else send(1,2,(epoch+k)%8,(epoch*3+k)%32,epoch*7+k);
      end
      send(3);synchronized_to(4+epoch);
    end
    if (busy_cycles<accepted_updates) $fatal(1,"REPLAY_FAIL: replay skipped recorded updates");
    if (!reads_during_replay || !reads_on_swap) $fatal(1,"REPLAY_FAIL: missing overlapping lookup coverage");
    $display("REPLAY_CONTROLLER_PASS cycles=%0d commits=%0d mapper_updates=%0d busy_cycles=%0d lookups=%0d lookups_during_replay=%0d lookups_on_swap=%0d",cycles,swaps,accepted_updates,busy_cycles,checked_reads,reads_during_replay,reads_on_swap);
    $finish;
  end
endmodule
