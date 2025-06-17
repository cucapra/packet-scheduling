#include <iostream>
#include <queue>
#include <cstdlib> 
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vflow_scheduler.h"

#define NUM_CMDS   50
#define QUEUE_SIZE 10
#define WAVEFORM   "waveform.vcd"

enum class Op { Push, Pop };

struct RankValue {
    int rank;
    int value;
    int counter; // to break ties in FIFO order 

    bool operator()(const RankValue l, const RankValue r) {
        if (l.rank == r.rank) 
            return l.counter > r.counter;

        return l.rank > r.rank;
    }
};

struct Cmd {
    Op op;
    RankValue data;
};


std::vector<Cmd> generate_commands(int num_cmds) {
    std::vector<Cmd> cmds;
    int size = 0;
    
    for (int i = 0; i < num_cmds; i++) {
        Cmd cmd;
        if (!size || size == QUEUE_SIZE) // to avoid over/underflow
            cmd.op = !size ? Op::Push : Op::Pop;
        else 
            cmd.op = rand() % 3 ? Op::Push : Op::Pop;
        int v = rand() % 1000, r = rand() % 1000;
        cmd.data = {r, v, i};
        cmds.push_back(cmd);
        size = size + (cmd.op == Op::Push ? 1 : -1);
    }

    while (size) {
        Cmd cmd = { .op = Op::Pop };
        cmds.push_back(cmd);
        size--;
    }

    return cmds;
}


std::vector<int> compute_expected(std::vector<Cmd> cmds) {
    std::priority_queue<RankValue, std::vector<RankValue>, RankValue> pifo;
    std::vector<int> out;

    for (Cmd cmd : cmds) {
        switch (cmd.op) {
            case Op::Push:
                pifo.push(cmd.data);
                break;

            case Op::Pop:
                out.push_back(pifo.top().value);
                pifo.pop();
                break;
        }
    }
    
    return out;
}


std::vector<int> simulate(std::vector<Cmd> cmds, const char* waveform) {
    Vflow_scheduler *dut = new Vflow_scheduler;
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
            if (dut->pop_valid) 
                out.push_back(dut->pop_value);

            if (it == cmds.end()) {
                dut->push = 0;
                dut->pop = 0;
                delay--;
                continue;
            }

            Cmd cmd = *it;

            dut->push = cmd.op == Op::Push;
            dut->pop  = cmd.op == Op::Pop;
            dut->push_rank  = cmd.data.rank;
            dut->push_value = cmd.data.value;

            it++;
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
            std::cout << argv[0] << " [-h] [-v] [-n NUM_CMDS] [-w FILE.vcd]" << std::endl;
            return 0;
        }
    }

    std::vector<Cmd> cmds   = generate_commands(num_cmds);
    std::vector<int> expect = compute_expected(cmds);
    std::vector<int> output = simulate(cmds, waveform);
    
    if (verbose) {
        std::cout << "Commands" << std::endl;
        for (Cmd c : cmds) {
            int value = c.data.value, rank = c.data.rank;
            if (c.op == Op::Push)
                std::cout << "push(" << value << ", " << rank << ")" << std::endl;
            else
                std::cout << "pop" << std::endl;
        }
        std::cout << "Expected" << std::endl;
        for (int i : expect) std::cout << i << std::endl;

        std::cout << "Actual" << std::endl;
        for (int i : output) std::cout << i << std::endl;
    }

    if (expect == output)
        std::cout << "\x1B[32mTEST PASS\033[0m\t\t" << std::endl;
    else
        std::cout << "\x1B[31mTEST FAIL\033[0m\t\t" << std::endl;

    return 0;
}
