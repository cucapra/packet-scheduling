`ifndef RANK_STORE
`define RANK_STORE


module RankStore #(parameter SIZE = 50, parameter FLOWS = 10) (
    input  clk,
    input  rst,
        
    // For enqueue

    input              push,
    input  [31:0]      push_rank,
    input  [31:0]      push_value,
    input  [FLOWS-1:0] push_flow,

    // For dequeue

    input              pop,
    input  [FLOWS-1:0] pop_flow,
    output [31:0]      pop_value,
    output [31:0]      pop_rank,
    output             pop_valid
);

    logic [FLOWS - 1 : 0] [SIZE - 1 : 0] [31:0] values;
    logic [FLOWS - 1 : 0] [SIZE - 1 : 0] [31:0] ranks;

    logic [FLOWS - 1 : 0] [SIZE - 1 : 0] head;
    logic [FLOWS - 1 : 0] [SIZE - 1 : 0] tail;

    always_ff @( posedge clk ) begin
        if ( rst ) begin
            for ( int i = 0; i < FLOWS; i++ ) begin
                head[i] <= 1;
                tail[i] <= 1;
            end
        end

        if ( push && !rst ) 
            for ( int i = 0; i < FLOWS; i++ )
                for ( int j = 0; j < SIZE; j++ )
                    if ( push_flow[i] && tail[i][j] ) begin
                        values[i][j] <= push_value;
                        ranks[i][j]  <= push_rank;
                        if ( tail[i][SIZE - 1] )
                            tail[i] <= 1 ;
                        else
                            tail[i] <= tail[i] << 1;
                    end

        if ( pop & !rst )
            for ( int i = 0; i < FLOWS; i++ )
                for ( int j = 0; j < SIZE; j++ )
                    if ( pop_flow[i] && head[i][j] ) begin
                        out_value <= values[i][j];
                        out_rank  <= ranks[i][j];
                        if ( head[i][SIZE - 1] )
                            head[i] <= 1;
                        else
                            head[i] <= head[i] << 1;
                    end

        out_valid <= pop;
    end

    logic [31:0] out_value;
    logic [31:0] out_rank;
    logic        out_valid;

    assign pop_value = out_value;
    assign pop_rank  = out_rank;
    assign pop_valid = out_valid;
endmodule

`endif /* RANK_STORE */
