`ifndef FLOW_SCHEDULER
`define FLOW_SCHEDULER

//                     requires  SIZE > 1
module FlowScheduler #(parameter SIZE = 10, parameter FLOWS = 10) (
    input  clk,
    input  rst,
        
    // For enqueue

    input              push_1,
    input  [31:0]      push_rank_1,
    input  [31:0]      push_value_1,
    input  [FLOWS-1:0] push_flow_1,

    input              push_2,
    input  [31:0]      push_rank_2,
    input  [31:0]      push_value_2,
    input  [FLOWS-1:0] push_flow_2,

    output             can_push_1,
    output             can_push_2,

    // For dequeue

    input              pop,
    output [31:0]      pop_value,
    output [FLOWS-1:0] pop_flow,
    output             pop_valid,
    output             can_pop
);

    logic [SIZE - 1 : 0] [31:0]      values;
    logic [SIZE - 1 : 0] [31:0]      ranks;
    logic [SIZE - 1 : 0] [FLOWS-1:0] flows;
    logic [SIZE - 1 : 0]             valids;
    logic [31:0] size;

    assign can_push_1 = size < SIZE;
    assign can_push_2 = size < SIZE - 1;
    assign can_pop    = size != 0;

    //-------------------------------------------------------------------------
    // Check stage
    //-------------------------------------------------------------------------

    logic [1:0]  [31:0]      push_value_C;
    logic [1:0]  [31:0]      push_rank_C;
    logic [1:0]  [FLOWS-1:0] push_flow_C;
    logic [1:0]              push_valid_C;
    logic signed [SIZE:0]    insert_idx_C [1:0];

    assign push_valid_C[0] = push_1;
    assign push_value_C[0] = push_value_1;
    assign push_rank_C[0]  = push_rank_1;
    assign push_flow_C[0]  = push_flow_1;

    assign push_valid_C[1] = push_2;
    assign push_value_C[1] = push_value_2;
    assign push_rank_C[1]  = push_rank_2;
    assign push_flow_C[1]  = push_flow_2;

    logic pop_valid_C;
    assign pop_valid_C = pop;

    // compute index to insert new element for `push_1` and `push_2`
    always_comb begin
        for ( int j = 0; j < 2; j++ ) begin
            insert_idx_C[j][SIZE] = 1;
            for ( int i = 0; i < SIZE; i++ )
                insert_idx_C[j][i] = !valids[i] || push_rank_C[j] < ranks[i];
        end

        for ( int c = 0; c < 2; c++ ) begin
            for ( int s = 1; s >= 0; s-- )
                if ( push_valid_S[s] &&
                   ( insert_idx_S[s] >  insert_idx_C[c] ||
                   ( insert_idx_S[s] == insert_idx_C[c] && push_rank_S[s] <= push_rank_C[c] )))
                    insert_idx_C[c] = insert_idx_C[c] << 1;

            if ( pop_valid_S && !insert_idx_C[c][0] )
                insert_idx_C[c] = insert_idx_C[c] >>> 1;
        end
    end

    //-------------------------------------------------------------------------
    // Shift stage
    //-------------------------------------------------------------------------

    logic [1:0] [31:0]      push_value_S;
    logic [1:0] [31:0]      push_rank_S;
    logic [1:0] [FLOWS-1:0] push_flow_S;
    logic [1:0]             push_valid_S;
    logic       [SIZE:0]    insert_idx_S [1:0];

    logic pop_valid_S;
    
    always_ff @( posedge clk ) begin 
        // reset + propogate
        if ( rst ) begin
            push_valid_S <= 0;
            pop_valid_S  <= 0;
            for ( int i = 0; i < SIZE; i++ ) valids[i] <= 0;
        end
        else begin
            // swap if push_2's rank < push_1's rank
            for ( int i = 0; i < 2; i++ ) begin
                if ( push_valid_C[1] && push_rank_C[1] <  push_rank_C[0] ) begin
                    insert_idx_S[i] <= insert_idx_C[1 - i];
                    push_value_S[i] <= push_value_C[1 - i];
                    push_rank_S[i]  <= push_rank_C[1 - i];
                    push_flow_S[i]  <= push_flow_C[1 - i];
                end
                else begin
                    insert_idx_S[i] <= insert_idx_C[i];
                    push_value_S[i] <= push_value_C[i];
                    push_rank_S[i]  <= push_rank_C[i];
                    push_flow_S[i]  <= push_flow_C[i];
                end
            end

            push_valid_S <= push_valid_C;
            pop_valid_S  <= pop_valid_C;
             
            if ( push_valid_C[0] && push_valid_C[1] ) 
                size <= size + 2;
            else if ( push_valid_C[0] ) 
                size <= size + 1;
            if ( pop_valid_C ) 
                size <= size - 1;
        end

        // insert + shift
        if ( push_valid_S[1] && !rst ) begin // perform push 2
            // shifting
            for ( int i = 0; i < SIZE - 2; i++ ) begin
                if ( insert_idx_S[1][i] ) begin
                    values[i + 2] <= values[i];
                    ranks[i + 2]  <= ranks[i];
                    flows[i + 2]  <= flows[i];
                    valids[i + 2] <= valids[i];
                end
            end
            
            // insert
            for ( int i = 1; i < SIZE; i++ )
                if ( insert_idx_S[1][i] && !insert_idx_S[1][i - 1] ) begin
                    values[i + 1] <= push_value_S[1];
                    ranks[i + 1]  <= push_rank_S[1];
                    flows[i + 1]  <= push_flow_S[1];
                    valids[i + 1] <= 1;
                end
            if ( insert_idx_S[1][0] == 1 ) begin
                values[1] <= push_value_S[1];
                ranks[1]  <= push_rank_S[1];
                flows[1]  <= push_flow_S[1];
                valids[1] <= 1;
            end
        end

        if ( push_valid_S[0] && !rst ) begin // perform push 1
            // shift
            for ( int i = 0; i < SIZE - 1; i++ ) begin
                if ( insert_idx_S[0][i] && ( !push_valid_S[1] || !insert_idx_S[1][i]) ) begin
                    values[i + 1] <= values[i];
                    ranks[i + 1]  <= ranks[i];
                    flows[i + 1]  <= flows[i];
                    valids[i + 1] <= valids[i];
                end
            end

            // insert
            for ( int i = 1; i < SIZE; i++ )
                if ( insert_idx_S[0][i] && !insert_idx_S[0][i - 1] ) begin
                    values[i] <= push_value_S[0];
                    ranks[i]  <= push_rank_S[0];
                    flows[i]  <= push_flow_S[0];
                    valids[i] <= 1;
                end
            if ( insert_idx_S[0][0] == 1 ) begin
                values[0] <= push_value_S[0];
                ranks[0]  <= push_rank_S[0];
                flows[0]  <= push_flow_S[0];
                valids[0] <= 1;
            end
        end

        if ( pop_valid_S && !rst ) begin // perform pop
            // shift
            valids[SIZE - 1] <= 0;
            for ( int i = 1; i < SIZE; i++ ) begin
                values[i - 1] <= values[i];
                ranks[i - 1]  <= ranks[i];
                flows[i - 1]  <= flows[i];
                valids[i - 1] <= valids[i];
            end
        end
    end
    
    assign pop_value = values[0];
    assign pop_flow  = flows[0];
    assign pop_valid = pop_valid_S;
endmodule

`endif /* FLOW_SCHEDULER */
