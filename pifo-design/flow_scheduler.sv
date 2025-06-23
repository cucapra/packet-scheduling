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

    always_ff @( posedge clk ) begin
        if ( rst ) begin
            push_valid_S <= 0;
            pop_valid_S  <= 0;
            for ( int i = 0; i < N; i++ ) elements[i].valid <= 0;
        end
    end

    assign is_full  = size == N;
    assign is_empty = size == 0;

    //-------------------------------------------------------------------------
    // Check stage
    //-------------------------------------------------------------------------

    logic [1:0] [31:0] push_value_C;
    logic [1:0] [31:0] push_rank_C;
    logic [1:0]        push_valid_C;
    logic [1:0] [31:0] insert_idx_C;

    assign push_valid_C[0] = push_1;
    assign push_value_C[0] = push_value_1;
    assign push_rank_C[0]  = push_rank_1;

    assign push_valid_C[1] = push_2;
    assign push_value_C[1] = push_value_2;
    assign push_rank_C[1]  = push_rank_2;

    logic pop_valid_C;
    assign pop_valid_C = pop;
    
    // compute index to insert new element for `push_1`
    always_comb begin
        insert_idx_C[0] = N - 1;
        insert_idx_C[1] = N - 1;
       
        for ( int j = 0; j < 2; j++ )
            for ( int i = 0; i < N; i++ )
                if ( !elements[i].valid || push_rank_C[j] < elements[i].rank ) begin
                    insert_idx_C[j] = i;
                    for ( int k = 0; k < 2; k++ ) begin
                        if ( push_valid_S[k] && insert_idx_S[k] < i )
                            insert_idx_C[j] += 1;
                        if ( push_valid_S[k]      && 
                             insert_idx_S[k] == i && 
                             push_rank_S[k]  <= push_rank_C[j] )
                            insert_idx_C[j] += 1;
                    end

                    if ( pop_valid_S && insert_idx_C[j] > 0 )
                        insert_idx_C[j] -= 1;

                    break;
                end
    end

    //-------------------------------------------------------------------------
    // Shift stage
    //-------------------------------------------------------------------------

    logic [1:0] [31:0] push_value_S;
    logic [1:0] [31:0] push_rank_S;
    logic [1:0]        push_valid_S;
    logic [1:0] [31:0] insert_idx_S;

    logic pop_valid_S;
    
    // propogate
    always_ff @( posedge clk ) begin
        insert_idx_S <= insert_idx_C;
        push_value_S <= push_value_C;
        push_rank_S  <= push_rank_C;
        push_valid_S <= push_valid_C;
        pop_valid_S  <= pop_valid_C;
    end
    
    // shift
    always_ff @( posedge clk ) begin 
        if ( push_valid_S[0] && push_valid_S[1] ) begin
            // perform two pushes 
            for ( int i = 0; i < N - 1; i++ ) begin
                if ( insert_idx_S[0] <= i && insert_idx_S[1] <= i) 
                    elements[i + 2] <= elements[i];
                else if ( insert_idx_S[0] <= i || insert_idx_S[1] <= i )
                    elements[i + 1] <= elements[i];
            end

            if ( insert_idx_S[0] == insert_idx_S[1] ) begin
                elements[insert_idx_S[0]].valid <= 1;
                elements[insert_idx_S[0]].value <= 
                    push_rank_S[0] <= push_rank_S[1] ? push_value_S[0] : push_value_S[1];
                elements[insert_idx_S[0]].rank  <= 
                    push_rank_S[0] <= push_rank_S[1] ? push_rank_S[0] : push_rank_S[1];

                elements[insert_idx_S[1] + 1].valid <= 1;
                elements[insert_idx_S[1] + 1].value <= 
                    push_rank_S[0] <= push_rank_S[1] ? push_value_S[1] : push_value_S[0];
                elements[insert_idx_S[1] + 1].rank  <= 
                    push_rank_S[0] <= push_rank_S[1] ? push_rank_S[1] : push_rank_S[0];
            end
            else if ( insert_idx_S[0] < insert_idx_S[1] ) begin
                elements[insert_idx_S[0]].valid <= 1;
                elements[insert_idx_S[0]].value <= push_value_S[0];
                elements[insert_idx_S[0]].rank  <= push_rank_S[0];

                elements[insert_idx_S[1] + 1].valid <= 1;
                elements[insert_idx_S[1] + 1].value <= push_value_S[1];
                elements[insert_idx_S[1] + 1].rank  <= push_rank_S[1];
            end
            else begin
                elements[insert_idx_S[0] + 1].valid <= 1;
                elements[insert_idx_S[0] + 1].value <= push_value_S[0];
                elements[insert_idx_S[0] + 1].rank  <= push_rank_S[0];

                elements[insert_idx_S[1]].valid <= 1;
                elements[insert_idx_S[1]].value <= push_value_S[1];
                elements[insert_idx_S[1]].rank  <= push_rank_S[1];
            end

            size <= size + 2;
        end
        else if ( push_valid_S[1] && pop_valid_S ) begin
            // perform push and pop (first push, then pop)
            if ( insert_idx_S[1] != 0 ) begin
                elements[insert_idx_S[1] - 1].valid <= 1;
                elements[insert_idx_S[1] - 1].value <= push_value_S[1];
                elements[insert_idx_S[1] - 1].rank  <= push_rank_S[1];

                for ( int i = 1; i < N; i++ )
                    if ( i < insert_idx_S[1] ) elements[i - 1] <= elements[i];
            end
        end
        else if ( push_valid_S[0] ) begin
            // perform push
            elements[insert_idx_S[0]].valid <= 1;
            elements[insert_idx_S[0]].value <= push_value_S[0];
            elements[insert_idx_S[0]].rank  <= push_rank_S[0];

            for ( int i = 0; i < N - 1; i++ )
                if ( insert_idx_S[0] <= i ) elements[i + 1] <= elements[i];

            size <= size + 1;
        end
        else if ( pop_valid_S ) begin
            // perform pop
            for ( int i = 1; i < N; i++ ) elements[i - 1] <= elements[i];
            elements[N - 1].valid <= 0;

            size <= size - 1;
        end
    end
    
    assign pop_value = ( push_valid_S[1] && insert_idx_S[1] == 0 ) ? push_value_S[1] : elements[0].value;
    assign pop_valid = pop_valid_S;
endmodule

`endif /* FLOW_SCHEDULER */
