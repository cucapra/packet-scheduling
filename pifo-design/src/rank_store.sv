`ifndef RANK_STORE
`define RANK_STORE


module RankStore #(parameter FLOWS = 10, parameter SIZE = 50) (
    input  clk,
    input  rst,
        
    // For enqueue

    input              push,
    input  [31:0]      push_value,
    input  [FLOWS-1:0] push_flow,

    // For dequeue

    input              pop,
    input  [FLOWS-1:0] pop_flow,
    output [31:0]      pop_value,
    output             pop_valid
);

    logic [31:0] bank [FLOWS - 1 : 0] [SIZE - 1 : 0];

    logic [SIZE - 1 : 0] head [FLOWS - 1 : 0];
    logic [SIZE - 1 : 0] tail [FLOWS - 1 : 0];

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
                        bank[i][j] <= push_value;
                        tail[i]    <= tail[i] << 1;
                        if ( tail[i][SIZE - 1] )
                            tail[i] <= 1;
                        else
                            tail[i] <= tail[i] << 1;
                    end

        if ( pop & !rst )
            for ( int i = 0; i < FLOWS; i++ )
                for ( int j = 0; j < SIZE; j++ )
                    if ( pop_flow[i] && head[i][j] ) begin
                        out_value <= bank[i][j];
                        if ( head[i][SIZE - 1] )
                            head[i] <= 1;
                        else
                            head[i] <= head[i] << 1;
                    end

        out_valid <= pop;
    end

    logic [31:0] out_value;
    logic        out_valid;

    assign pop_value = out_value;
    assign pop_valid = out_valid;
endmodule

`endif /* RANK_STORE */
