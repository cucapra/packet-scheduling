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

    logic [FLOWS - 1 : 0] empty;
    logic [FLOWS - 1 : 0] full;

    logic [FLOWS - 1 : 0] allow_push;
    logic [FLOWS - 1 : 0] allow_pop;
    logic                 any_pop_valid;

    // invalidate pushes that overflow and pops that underflow
    always_comb begin
        any_pop_valid = 0;
        for ( int i = 0; i < FLOWS; i++ ) begin
            allow_push[i] = push && push_flow[i] && ( !full[i]  || ( pop  && pop_flow[i]  ) );
            allow_pop[i]  = pop  && pop_flow[i]  && ( !empty[i] || ( push && push_flow[i] ) );
            any_pop_valid = any_pop_valid || allow_pop[i];
        end
    end

    always_ff @( posedge clk ) begin
        if ( rst ) begin
            for ( int i = 0; i < FLOWS; i++ ) begin
                head[i]  <= 1;
                tail[i]  <= 1;
                empty[i] <= 1;
                full[i]  <= 0;
            end
        end

        for ( int i = 0; i < FLOWS; i++ )
            for ( int j = 0; j < SIZE; j++ ) begin
                if ( allow_push[i] && tail[i][j] ) begin
                    values[i][j] <= push_value;
                    ranks[i][j]  <= push_rank;

                    if ( tail[i][SIZE - 1] ) begin
                        tail[i]  <= 1;
                        if ( !allow_pop[i] ) full[i] <= head[i][0];
                    end
                    else begin
                        tail[i] <= tail[i] << 1;
                        if ( !allow_pop[i] ) full[i] <= head[i][j + 1];
                    end

                    if ( !allow_pop[i] ) empty[i] <= 0;
                end

                if ( allow_pop[i] && head[i][j] ) begin
                    if ( allow_push[i] && empty[i] ) begin
                        out_value <= push_value;
                        out_rank  <= push_rank;
                    end
                    else begin
                        out_value <= values[i][j];
                        out_rank  <= ranks[i][j];
                    end

                    if ( head[i][SIZE - 1] ) begin
                        head[i] <= 1;
                        if ( !allow_push[i] ) empty[i] <= tail[i][0];
                    end
                    else begin
                        head[i] <= head[i] << 1;
                        if ( !allow_push[i] ) empty[i] <= tail[i][j + 1];
                    end

                    if ( !allow_push[i] ) full[i] <= 0;
                end
            end

        out_valid <= any_pop_valid;
    end

    logic [31:0] out_value;
    logic [31:0] out_rank;
    logic        out_valid;

    assign pop_value = out_value;
    assign pop_rank  = out_rank;
    assign pop_valid = out_valid;
endmodule

`endif /* RANK_STORE */
