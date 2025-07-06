#include <iostream>
#include <queue>
#include <cstdlib> 
#include <cstdio> 
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vpifo.h"

#define NUM_CMDS   50
#define FLOWS      10
#define SIZE       51
#define WAVEFORM   "pifo.vcd"
#define POP_GAP    3 // in cycles

#define LOG(x) (sizeof(unsigned int) * 8 - __builtin_clz(x) - 1)

enum class Op { Push, Pop, Nop };

struct Node {
    unsigned int rank;
    unsigned int flow;
    unsigned int value;

    // weird logic to break rank ties
    int  cycle; 
    bool prio;

    bool operator()(const Node& l, const Node& r) {
        if (l.rank == r.rank && l.cycle == r.cycle) return l.prio;
        if (l.rank == r.rank) return l.cycle > r.cycle;

        return l.rank > r.rank;
    }
};

struct Cmd {
    Op op;
    Node data;
};


std::vector<Cmd> generate_commands(int num_cmds) {
    std::vector<Cmd> cmds;
    int size  = 0;
    int delay = POP_GAP;
    for (int i = 0; i < num_cmds; i++) {
        Cmd cmd;
        if (size == SIZE) // to avoid overflow
            cmd.op = Op::Pop;
        else if (!size)   // to avoid underflow
            cmd.op = Op::Push;
        else {
            switch (rand() % 3) {
                case 0:
                    cmd.op = Op::Push;
                    break;
                case 1:
                    cmd.op = Op::Pop;
                    break;
                case 2:
                    cmd.op = Op::Nop;
                    break;
            }
        }
        unsigned int v = rand() % 1000, 
                     r = rand() % 1000, 
                     f = 1U << (rand() % FLOWS);
        cmd.data = {r, f, v};
        if (cmd.op == Op::Pop && delay > 0) cmd.op = Op::Nop;
        cmds.push_back(cmd);


        int delta;
        switch (cmd.op) {
            case Op::Push: delta = 1;  break;
            case Op::Pop:  delta = -1; break;
            case Op::Nop:  delta = 0;  break;
        }
        delay = cmd.op == Op::Pop ? POP_GAP : delay - 1;
        size = size + delta;
    }

    while (size) {
        Cmd cmd;
        if (delay > 0) {
            cmd = { .op = Op::Nop };
            delay--;
        }
        else {
            cmd = { .op = Op::Pop };
            delay = POP_GAP;
            size--;
        }
        cmds.push_back(cmd);
    }

    return cmds;
}


std::vector<unsigned int> compute_expected(std::vector<Cmd> cmds) {
    std::queue<Node> rs[FLOWS];
    std::priority_queue<Node, std::vector<Node>, Node> fs;
    int size[FLOWS] = {0};
    std::vector<unsigned int> out;
    
    int idx;
    Node node;
    int i = 0;
    for (Cmd cmd : cmds) {
        switch (cmd.op) {
            case Op::Push:
                idx = LOG(cmd.data.flow);
                if (size[idx])
                    rs[idx].push(cmd.data);
                else {
                    cmd.data.cycle = i;
                    cmd.data.prio  = true;
                    fs.push(cmd.data);
                }
                size[idx]++;
                break;

            case Op::Pop:
                node = fs.top();
                fs.pop();
                idx = LOG(node.flow);
                out.push_back(node.value);
                size[idx]--;
                if (size[idx]) {
                    node = rs[idx].front();
                    node.cycle = i + 2;
                    node.prio  = false;
                    fs.push(node);
                    rs[idx].pop();
                }
                break;

            case Op::Nop:
                // nothing to do...
                break;
        }

        i++;
    }
    
    return out;
}


std::vector<unsigned int> simulate(std::vector<Cmd> cmds, const char* waveform) {
    Vpifo *dut = new Vpifo;
    std::vector<unsigned int> out;

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
                dut->push = 0;
                dut->pop  = 0;
                delay--;
            }
            else {
                Cmd cmd = *it;
                
                dut->push = cmd.op == Op::Push;
                dut->pop  = cmd.op == Op::Pop;

                dut->push_rank  = cmd.data.rank;
                dut->push_value = cmd.data.value;
                dut->push_flow  = cmd.data.flow;

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

    std::vector<Cmd>          cmds   = generate_commands(num_cmds);
    std::vector<unsigned int> expect = compute_expected(cmds);
    std::vector<unsigned int> output = simulate(cmds, waveform);
    
    if (verbose) {
        std::cout << "Commands" << std::endl;
        for (Cmd c : cmds) {
            unsigned int v = c.data.value, 
                         r = c.data.rank, 
                         f = c.data.flow;
            switch (c.op) {
                case Op::Push:
                    printf("push(v=%u, f=%u, r=%u)\n", v, f, r);
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
        for (auto x : expect) printf("%u\n", x);

        std::cout << "Output" << std::endl;
        for (auto x : output) printf("%u\n", x);
    }

    if (expect == output)
        std::cout << "\x1B[32mTEST PASS\033[0m\t\t" << std::endl;
    else {
        std::cout << "\x1B[31mTEST FAIL\033[0m\t\t" << std::endl;
        return 1;
    }

    return 0;
}
