`ifndef FLOW_SCHEDULER
`define FLOW_SCHEDULER


module FlowScheduler #(parameter N = 10) (
    input  clk,
    input  rst,
        
    // For enqueue

    input         push_1,
    input  [31:0] push_rank_1,
    input  [31:0] push_value_1,

    input         push_2,
    input  [31:0] push_rank_2,
    input  [31:0] push_value_2,

    output        is_full,

    // For dequeue

    input         pop,
    output [31:0] pop_value,
    output        pop_valid,
    output        is_empty
);

    typedef struct {
        logic [31:0] value;
        logic [31:0] rank;
        logic        valid;
    } element;

    element elements [N - 1 : 0];
    logic [31:0] size;

    assign is_full  = size == N;
    assign is_empty = size == 0;

    //-------------------------------------------------------------------------
    // Check stage
    //-------------------------------------------------------------------------

    logic [1:0]  [31:0] push_value_C;
    logic [1:0]  [31:0] push_rank_C;
    logic [1:0]         push_valid_C;
    logic signed [N :0] insert_idx_C [1:0];

    assign push_valid_C[0] = push_1;
    assign push_value_C[0] = push_value_1;
    assign push_rank_C[0]  = push_rank_1;

    assign push_valid_C[1] = push_2;
    assign push_value_C[1] = push_value_2;
    assign push_rank_C[1]  = push_rank_2;

    logic pop_valid_C;
    assign pop_valid_C = pop;

    // compute index to insert new element for `push_1` and `push_2`
    always_comb begin
        for ( int j = 0; j < 2; j++ ) begin
            insert_idx_C[j][N] = 1;
            for ( int i = 0; i < N; i++ )
                insert_idx_C[j][i] = !elements[i].valid || push_rank_C[j] < elements[i].rank;
        end

        for ( int c = 0; c < 2; c++ ) begin
            for ( int s = 0; s < 2; s++ )
                if (  push_valid_S[s] &&
                    ( insert_idx_S[s] > insert_idx_C[c] ||
                    ( insert_idx_S[s] == insert_idx_C[c] && push_rank_S[s] <= push_rank_C[c] ) ) )
                    insert_idx_C[c] = insert_idx_C[c] << 1;

            if ( pop_valid_S && !insert_idx_C[c][0] )
                insert_idx_C[c] = insert_idx_C[c] >>> 1;
        end
    end

    //-------------------------------------------------------------------------
    // Shift stage
    //-------------------------------------------------------------------------

    logic [1:0] [31:0] push_value_S;
    logic [1:0] [31:0] push_rank_S;
    logic [1:0]        push_valid_S;
    logic       [N :0] insert_idx_S [1:0];

    logic pop_valid_S;
    
    // propogate + reset
    always_ff @( posedge clk ) begin
        if ( rst ) begin
            push_valid_S <= 0;
            pop_valid_S  <= 0;
            for ( int i = 0; i < N; i++ ) elements[i].valid <= 0;
        end
        else begin
            for ( int i = 0; i < 2; i++ ) begin
                if ( insert_idx_C[0] > insert_idx_C[1] && 
                     push_valid_C[0] && push_valid_C[1] ) begin
                    insert_idx_S[i] <= insert_idx_C[1 - i];
                    push_value_S[i] <= push_value_C[1 - i];
                    push_rank_S[i]  <= push_rank_C[1 - i];
                end
                else begin
                    insert_idx_S[i] <= insert_idx_C[i];
                    push_value_S[i] <= push_value_C[i];
                    push_rank_S[i]  <= push_rank_C[i];
                end
            end

            push_valid_S <= push_valid_C;
            pop_valid_S <= pop_valid_C;
        end
    end
    
    // insert + shift
    always_ff @( posedge clk ) begin 
        if ( push_valid_S[0] && push_valid_S[1] ) begin // perform two pushes 
            // shifting
            for ( int i = 0; i < N - 1; i++ ) begin
                if ( i < N - 2 && insert_idx_S[0][i] && insert_idx_S[1][i] ) 
                    elements[i + 2] <= elements[i];
                else if ( insert_idx_S[0][i] || insert_idx_S[1][i] )
                    elements[i + 1] <= elements[i];
            end
            
            // insert
            for ( int i = 1; i < N; i++ ) begin
                if ( insert_idx_S[0] == insert_idx_S[1] ) begin
                    if ( insert_idx_S[0][i] && !insert_idx_S[0][i - 1] ) begin
                        elements[i].valid <= 1;
                        elements[i].value <= 
                            push_rank_S[0] <= push_rank_S[1] ? push_value_S[0] : push_value_S[1];
                        elements[i].rank  <= 
                            push_rank_S[0] <= push_rank_S[1] ? push_rank_S[0] : push_rank_S[1];

                        elements[i + 1].valid <= 1;
                        elements[i + 1].value <= 
                            push_rank_S[0] <= push_rank_S[1] ? push_value_S[1] : push_value_S[0];
                        elements[i + 1].rank  <= 
                            push_rank_S[0] <= push_rank_S[1] ? push_rank_S[1] : push_rank_S[0];
                    end
                end
                else begin
                    if ( insert_idx_S[0][i] && !insert_idx_S[0][i - 1] ) begin
                        elements[i].valid <= 1;
                        elements[i].value <= push_value_S[0];
                        elements[i].rank  <= push_rank_S[0];
                    end
                    if ( insert_idx_S[1][i] && !insert_idx_S[1][i - 1] ) begin
                        elements[i + 1].valid <= 1;
                        elements[i + 1].value <= push_value_S[1];
                        elements[i + 1].rank  <= push_rank_S[1];
                    end
                end
            end

            size <= size + 2;
        end
        else if ( push_valid_S[0] ) begin // perform push
            // shift
            for ( int i = 0; i < N - 1; i++ ) begin
                if ( insert_idx_S[0][i] ) elements[i + 1] <= elements[i];
            end

            // insert
            for ( int i = 1; i < N; i++ )
                if ( insert_idx_S[0][i] && !insert_idx_S[0][i - 1] ) begin
                    elements[i].valid <= 1;
                    elements[i].value <= push_value_S[0];
                    elements[i].rank  <= push_rank_S[0];
                end
            if ( insert_idx_S[0][0] == 1 ) begin
                elements[0].valid <= 1;
                elements[0].value <= push_value_S[0];
                elements[0].rank  <= push_rank_S[0];
            end

            size <= size + 1;
        end
        else if ( pop_valid_S ) begin // perform pop
            // shift
            elements[N - 1].valid <= 0;
            for ( int i = 1; i < N; i++ ) elements[i - 1] <= elements[i];

            size <= size - 1;
        end
    end
    
    assign pop_value = elements[0].value;
    assign pop_valid = pop_valid_S;
endmodule

`endif /* FLOW_SCHEDULER */
