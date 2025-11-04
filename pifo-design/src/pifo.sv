`ifndef PIFO
`define PIFO

`include "flow_scheduler.sv"
`include "rank_store.sv"


module PIFO #(parameter BANK_SIZE = 50, parameter FLOWS = 10) (
    input  clk,
    input  rst,
        
    // For enqueue

    input              push,
    input  [31:0]      push_rank,
    input  [31:0]      push_value,
    input  [FLOWS-1:0] push_flow,

    // For dequeue

    input              pop,
    output [31:0]      pop_value,
    output             pop_valid
);

    logic             rs_push;
    logic [31:0]      rs_push_rank;
    logic [31:0]      rs_push_value;
    logic [FLOWS-1:0] rs_push_flow;

    logic             rs_pop;
    logic [FLOWS-1:0] rs_pop_flow;
    logic [31:0]      rs_pop_value;
    logic [31:0]      rs_pop_rank;
    logic             rs_pop_valid;

    RankStore#(BANK_SIZE, FLOWS) rank_store
    (
        .clk            (clk),
        .rst            (rst),

        .push           (rs_push),
        .push_rank      (rs_push_rank),
        .push_value     (rs_push_value),
        .push_flow      (rs_push_flow),

        .pop            (rs_pop),
        .pop_flow       (rs_pop_flow),
        .pop_value      (rs_pop_value),
        .pop_rank       (rs_pop_rank),
        .pop_valid      (rs_pop_valid)
    );

    logic             fs_push_1;
    logic [31:0]      fs_push_rank_1;
    logic [31:0]      fs_push_value_1;
    logic [FLOWS-1:0] fs_push_flow_1;

    logic             fs_push_2;
    logic [31:0]      fs_push_rank_2;
    logic [31:0]      fs_push_value_2;
    logic [FLOWS-1:0] fs_push_flow_2;

    logic             fs_pop;
    logic [31:0]      fs_pop_value;
    logic [FLOWS-1:0] fs_pop_flow;
    logic             fs_pop_valid;

    FlowScheduler#(FLOWS, FLOWS) flow_scheduler
    (
        .clk            (clk),
        .rst            (rst),
        
        // for packets from the rank store
        .push_1         (fs_push_1),
        .push_rank_1    (fs_push_rank_1),
        .push_value_1   (fs_push_value_1),
        .push_flow_1    (fs_push_flow_1),

        // for incoming packets 
        .push_2         (fs_push_2),
        .push_rank_2    (fs_push_rank_2),
        .push_value_2   (fs_push_value_2),
        .push_flow_2    (fs_push_flow_2),

        .pop            (fs_pop),
        .pop_value      (fs_pop_value),
        .pop_flow       (fs_pop_flow),
        .pop_valid      (fs_pop_valid)
    );

    logic             do_fs_push_2;
    logic             do_rs_push;
    logic [FLOWS-1:0] rs_pop_flow_prev;
    logic [FLOWS-1:0] flow_non_empty;
    
    // push incoming packet
    assign fs_push_2       = push && do_fs_push_2;
    assign fs_push_rank_2  = push_rank;
    assign fs_push_value_2 = push_value;
    assign fs_push_flow_2  = push_flow;
    assign rs_push         = push && do_rs_push;
    assign rs_push_rank    = push_rank;
    assign rs_push_value   = push_value;
    assign rs_push_flow    = push_flow;
        
    // push rank_store -> flow_scheduler
    assign fs_push_1       = rs_pop_valid;
    assign fs_push_rank_1  = rs_pop_rank;
    assign fs_push_value_1 = rs_pop_value;
    assign fs_push_flow_1  = rs_pop_flow_prev;

    // pop 
    assign fs_pop      = pop;
    assign rs_pop      = fs_pop_valid;
    assign rs_pop_flow = fs_pop_flow;
    assign pop_valid   = fs_pop_valid;
    assign pop_value   = fs_pop_value;

    always_comb begin
        do_rs_push   = 0;
        do_fs_push_2 = 0;
        for ( int i = 0; i < FLOWS; i++ ) begin
            do_rs_push   = do_rs_push   || (  (  flow_non_empty[i] || ( fs_push_1 &&  fs_push_flow_1[i]) ) && push_flow[i] );
            do_fs_push_2 = do_fs_push_2 || ( !(  flow_non_empty[i] || ( fs_push_1 &&  fs_push_flow_1[i]) ) && push_flow[i] );
        end
    end

    always_ff @( posedge clk ) begin 
        for ( int i = 0; i < FLOWS; i++ ) begin
            if (( fs_push_1 && fs_push_flow_1[i] ) || ( fs_push_2 && fs_push_flow_2[i] )) 
                flow_non_empty[i] <= 1;
            else if ( fs_pop_valid && fs_pop_flow[i] )
                flow_non_empty[i] <= 0;
        end

        rs_pop_flow_prev <= rs_pop_flow;
    end
endmodule

`endif /* PIFO */
