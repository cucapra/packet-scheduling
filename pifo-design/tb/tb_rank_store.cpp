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

enum class Op { Push, Pop };

struct Cmd {
    Op  op;
    int value;
    int flow;
};


std::vector<Cmd> generate_commands(int num_cmds) {
    std::vector<Cmd> cmds;
    int size[FLOWS] = {0};

    for (int i = 0; i < num_cmds; i++) {
        Cmd cmd;
        int v = rand() % 1000, f = rand() % 10;
        f = f < 0 ? -1 * f : f;

        int delta;
        if (size[f] == SIZE) { // to avoid overflow
            cmd.op = Op::Pop;
            delta = -1;
        }
        else if (!size[f]) {   // to avoid underflow
            cmd.op = Op::Push;
            delta = 1;
        }
        else {
            int rng = rand() % 2;
            rng = rng < 0 ? -1 * rng : rng;
            switch (rng) {
                case 0:
                    cmd.op = Op::Push;
                    delta  = 1;
                    break;
                case 1:
                    cmd.op = Op::Pop;
                    delta  = -1;
                    break;
            }
        }

        
        cmd.value = v;
        cmd.flow  = f;
        cmds.push_back(cmd);
        size[f]  += delta;
    }
    
    for (int f = 0; f <  FLOWS; f++)
        while (size[f]) {
            Cmd cmd = { .op = Op::Pop, .flow = f };
            cmds.push_back(cmd);
            size[f]--;
        }

    return cmds;
}


std::vector<int> compute_expected(std::vector<Cmd> cmds) {
    std::queue<int> bank[FLOWS];
    std::vector<int> out;

    for (Cmd cmd : cmds) {
        switch (cmd.op) {
            case Op::Push:
                bank[cmd.flow].push(cmd.value);
                break;

            case Op::Pop:
                out.push_back(bank[cmd.flow].front());
                bank[cmd.flow].pop();
                break;
        }
    }
    
    return out;
}


std::vector<int> simulate(std::vector<Cmd> cmds, const char* waveform) {
    Vrank_store *dut = new Vrank_store;
    std::vector<int> out;

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
            if (dut->pop_valid) out.push_back(dut->pop_value);

            if (it == cmds.end()) {
                dut->push       = 0;
                dut->pop        = 0;
                delay--;
            }
            else {
                Cmd cmd = *it;
                
                dut->push = cmd.op == Op::Push;
                dut->pop  = cmd.op == Op::Pop;
                
                int flow_1_hot  = 1 << cmd.flow;
                dut->push_value = cmd.value;
                dut->push_flow  = flow_1_hot;
                dut->pop_flow   = flow_1_hot;

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

    std::vector<Cmd> cmds   = generate_commands(num_cmds);
    std::vector<int> expect = compute_expected(cmds);
    std::vector<int> output = simulate(cmds, waveform);
    
    if (verbose) {
        std::cout << "Commands" << std::endl;
        for (Cmd c : cmds) {
            int value = c.value, flow = c.flow;
            switch (c.op) {
                case Op::Push:
                    printf("push(value=%d, flow=%d)\n", value, flow);
                    break;

                case Op::Pop:
                    printf("pop\n");
                    break;
            }
        }

        std::cout << "Expected" << std::endl;
        for (int i : expect) std::cout << i << std::endl;

        std::cout << "Output" << std::endl;
        for (int i : output) std::cout << i << std::endl;
    }

    if (expect == output)
        std::cout << "\x1B[32mTEST PASS\033[0m\t\t" << std::endl;
    else {
        std::cout << "\x1B[31mTEST FAIL\033[0m\t\t" << std::endl;
        return 1;
    }

    return 0;
}
