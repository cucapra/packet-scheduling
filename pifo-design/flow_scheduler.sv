`ifndef FLOW_SCHEDULER
`define FLOW_SCHEDULER


module FlowScheduler #(parameter N = 10) (
    input  clk,
    input  rst,
        
    // For enqueue

    input         push,
    input  [31:0] push_rank,
    input  [31:0] push_value,
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
            for ( int i = 0; i < N; i++ ) elements[i].valid <= 0;
        end
    end

    assign is_full  = size == N;
    assign is_empty = size == 0;

    //-------------------------------------------------------------------------
    // Check stage
    //-------------------------------------------------------------------------

    logic [31:0] push_value_C;
    logic [31:0] push_rank_C;
    logic        push_valid_C;
    logic [31:0] insert_idx_C;

    assign push_valid_C = push;
    assign push_value_C = push_value;
    assign push_rank_C  = push_rank;

    logic pop_valid_C;
    assign pop_valid_C = pop;
    
    // compute index to insert new element
    always_comb begin
        insert_idx_C = N - 1;

        for ( int i = 0; i < N; i++ )
            if ( !elements[i].valid || push_rank_C < elements[i].rank ) begin
                if ( push_valid_S && insert_idx_S < i )
                    insert_idx_C = i + 1;
                else if ( push_valid_S      && 
                          insert_idx_S == i && 
                          push_rank_S  <= push_rank_C )
                    insert_idx_C = i + 1;
                else if ( pop_valid_S && i > 0 )
                    insert_idx_C = i - 1;
                else
                    insert_idx_C = i;
                break;
            end
    end

    //-------------------------------------------------------------------------
    // Shift stage
    //-------------------------------------------------------------------------

    logic [31:0] push_value_S;
    logic [31:0] push_rank_S;
    logic        push_valid_S;
    logic [31:0] insert_idx_S;

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
        if ( push_valid_S && pop_valid_S ) begin
            // perform push and pop (first push, then pop)
            if ( insert_idx_S != 0 ) begin
                elements[insert_idx_S - 1].valid <= 1;
                elements[insert_idx_S - 1].value <= push_value_S;
                elements[insert_idx_S - 1].rank  <= push_rank_S;

                for ( int i = 1; i < N; i++ )
                    if ( i < insert_idx_S ) elements[i - 1] <= elements[i];
            end
        end
        else if ( push_valid_S ) begin
            // perform push
            elements[insert_idx_S].valid <= 1;
            elements[insert_idx_S].value <= push_value_S;
            elements[insert_idx_S].rank  <= push_rank_S;

            for ( int i = 0; i < N - 1; i++ )
                if ( insert_idx_S <= i ) elements[i + 1] <= elements[i];

            size <= size + 1;
        end
        else if ( pop_valid_S ) begin
            // perform pop
            for ( int i = 1; i < N; i++ ) elements[i - 1] <= elements[i];
            elements[N - 1].valid <= 0;

            size <= size - 1;
        end
    end
    
    assign pop_value = ( push_valid_S && insert_idx_S == 0 ) ? push_value_S : elements[0].value;
    assign pop_valid = pop_valid_S;
endmodule

`endif /* FLOW_SCHEDULER */
