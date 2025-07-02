#include <iostream>
#include <queue>
#include <cstdlib> 
#include <cstdio> 
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vrank_store.h"

#define NUM_CMDS   50
#define FLOWS      10
#define SIZE       50
#define WAVEFORM   "rank_store.vcd"

#define LOG(x) (sizeof(unsigned int) * 8 - __builtin_clz(x) - 1)

enum class Op { Push, Pop, Nop };

struct RankValue {
    unsigned int rank;
    unsigned int value;

    bool operator==(const RankValue& other) const {
        return rank == other.rank && value == other.value;
    }
};

struct Cmd {
    Op  op;
    unsigned int value;
    unsigned int flow;
    unsigned int rank;
};


std::vector<Cmd> generate_commands(int num_cmds) {
    std::vector<Cmd> cmds;
    int size[FLOWS] = {0};

    for (int i = 0; i < num_cmds; i++) {
        Cmd cmd;
        unsigned int v = rand() % 1000, r = rand() % 1000, f = rand() % FLOWS;

        if (size[f] == SIZE) // to avoid overflow
            cmd.op = Op::Pop;
        else if (!size[f])   // to avoid underflow
            cmd.op = Op::Push;
        else {
            int rng = rand() % 3;
            switch (rng) {
                case 0: cmd.op = Op::Push; break;
                case 1: cmd.op = Op::Pop;  break;
                case 2: cmd.op = Op::Nop;  break;
            }
        }

        cmd.value = v;
        cmd.rank  = r;
        cmd.flow  = 1U << f;
        cmds.push_back(cmd);

        int delta;
        switch (cmd.op) {
            case Op::Push: delta = 1;  break;
            case Op::Pop:  delta = -1; break;
            case Op::Nop:  delta = 0;  break;
        }
        size[f]  += delta;
    }
    
    for (unsigned int f = 0; f < FLOWS; f++)
        while (size[f]) {
            Cmd cmd = { .op = Op::Pop, .flow = 1U << f };
            cmds.push_back(cmd);
            size[f]--;
        }

    return cmds;
}


std::vector<RankValue> compute_expected(std::vector<Cmd> cmds) {
    std::queue<RankValue> bank[FLOWS];
    std::vector<RankValue> out;

    for (Cmd cmd : cmds) {
        switch (cmd.op) {
            case Op::Push:
                bank[LOG(cmd.flow)].push({ cmd.rank, cmd.value });
                break;

            case Op::Pop:
                out.push_back(bank[LOG(cmd.flow)].front());
                bank[LOG(cmd.flow)].pop();
                break;

            case Op::Nop:
                // nothing to do...
                break;
        }
    }
    
    return out;
}


std::vector<RankValue> simulate(std::vector<Cmd> cmds, const char* waveform) {
    Vrank_store *dut = new Vrank_store;
    std::vector<RankValue> out;

    Verilated::traceEverOn(true);
    VerilatedVcdC *m_trace = new VerilatedVcdC;
    dut->trace(m_trace, 5);
    m_trace->open(waveform);
    
    vluint64_t sim_time = 0;

    // 2 cycles of reset
    dut->rst = 1;
    for (int i = 0; i < 4; i++) {
        dut->clk ^= 1;
        dut->eval();
        m_trace->dump(sim_time);
        sim_time++;
    }
    dut->rst = 0;

    // process commands
    std::vector<Cmd>::iterator it = cmds.begin();
    int delay = 4; // pad with 4 cycles after all commands
    while (it != cmds.end() || delay) {
        dut->clk ^= 1;
        dut->eval();

        if (dut->clk) {
            if (dut->pop_valid) out.push_back({ dut->pop_rank, dut->pop_value });

            if (it == cmds.end()) {
                dut->push = 0;
                dut->pop  = 0;
                delay--;
            }
            else {
                Cmd cmd = *it;
                
                dut->push = cmd.op == Op::Push;
                dut->pop  = cmd.op == Op::Pop;
                
                dut->push_value = cmd.value;
                dut->push_rank  = cmd.rank;
                dut->push_flow  = cmd.flow;
                dut->pop_flow   = cmd.flow;

                it++;
            }
        }
        
        m_trace->dump(sim_time);
        sim_time++;
    }

    m_trace->close();
    delete dut;

    return out;
}


int main(int argc, char** argv, char** env) {
    int  num_cmds = NUM_CMDS;
    bool verbose  = false;
    const char* waveform = WAVEFORM;
    for (int i = 0; i < argc; i++) {
        if ( !strcmp(argv[i], "-v") ) 
            verbose = true;
        if ( !strcmp(argv[i], "-n") && i + 1 < argc )
            num_cmds = atoi(argv[i + 1]);
        if ( !strcmp(argv[i], "-w") && i + 1 < argc )
            waveform = argv[i + 1];
        if ( !strcmp(argv[i], "-h") ) {
            printf("%s [-h] [-v] [-n NUM_CMDS] [-w FILE.vcd]\n", argv[0]);
            return 0;
        }
    }

    std::vector<Cmd>       cmds   = generate_commands(num_cmds);
    std::vector<RankValue> expect = compute_expected(cmds);
    std::vector<RankValue> output = simulate(cmds, waveform);
    
    if (verbose) {
        std::cout << "Commands" << std::endl;
        for (Cmd c : cmds) {
            unsigned int value = c.value, flow = c.flow, rank = c.rank;
            switch (c.op) {
                case Op::Push:
                    printf("push(v=%u, f=%u, r=%u)\n", value, flow, rank);
                    break;

                case Op::Pop:
                    printf("pop\n");
                    break;

                case Op::Nop:
                    printf("nop\n");
                    break;
            }
        }

        std::cout << "Expected" << std::endl;
        for (auto x : expect) printf("v=%u\tr=%u\n", x.value, x.rank);

        std::cout << "Output" << std::endl;
        for (auto x : output) printf("v=%u\tr=%u\n", x.value, x.rank);
    }

    if (expect == output)
        std::cout << "\x1B[32mTEST PASS\033[0m\t\t" << std::endl;
    else {
        std::cout << "\x1B[31mTEST FAIL\033[0m\t\t" << std::endl;
        return 1;
    }

    return 0;
}
