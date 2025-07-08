#include <queue>
#include <vector>
#include <assert.h>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vpifo.h"

#include "tb.hpp"

#define FLOWS   10
#define SIZE    51
#define POP_GAP 3 // in cycles

#define WAVEFORM "pifo.vcd"

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

struct Output : public IOutput {
    unsigned int value;
    
    std::string to_string() const {
        return std::to_string(value);
    }

    bool is_equal(const IOutput& other) const {
        return value == static_cast<const Output&>(other).value;
    }
};

struct Cmd : public ICmd {
    Op op;
    Node data;

    std::string to_string() const {
        unsigned int v = data.value, 
                     r = data.rank, 
                     f = data.flow;

        char buff[100];
        switch (op) {
            case Op::Push:
                snprintf(buff, sizeof(buff), "push(v=%u, f=%u, r=%u)", v, f, r);
                break;

            case Op::Pop:
                snprintf(buff, sizeof(buff), "pop");
                break;

            case Op::Nop:
                snprintf(buff, sizeof(buff), "nop");
                break;

            default:
                assert(!"ERROR: invalid operation");
                exit(1);
        }

        return buff;
    }
};


std::vector<ICmd*> generate_commands(int num_cmds, bool overflow) {
    std::vector<ICmd*> cmds;
    int size = 0, delay = POP_GAP;

    for (int i = 0; i < num_cmds; i++) {
        Cmd* cmd = new Cmd;

        if (!overflow && size == SIZE) // to avoid overflow
            cmd->op = Op::Pop;
        else if (!overflow && !size)   // to avoid underflow
            cmd->op = Op::Push;
        else {
            switch (rand() % 3) {
                case 0:
                    cmd->op = Op::Push;
                    break;
                case 1:
                    cmd->op = Op::Pop;
                    break;
                case 2:
                    cmd->op = Op::Nop;
                    break;
            }
        }

        unsigned int v = rand() % 1000, 
                     r = rand() % 1000, 
                     f = 1U << (rand() % FLOWS);

        cmd->data = {r, f, v};
        if (cmd->op == Op::Pop && delay > 0) cmd->op = Op::Nop;
        cmds.push_back(cmd);


        int delta;
        switch (cmd->op) {
            case Op::Push: delta = 1;  break;
            case Op::Pop:  delta = -1; break;
            case Op::Nop:  delta = 0;  break;
        }
        delay = cmd->op == Op::Pop ? POP_GAP : delay - 1;
        size = size + delta;
    }
    
    // flush
    while (size > 0) {
        Cmd* cmd = new Cmd;
        if (delay > 0) {
            cmd->op = Op::Nop;
            delay--;
        }
        else {
            cmd->op = Op::Pop;
            delay   = POP_GAP;
            size--;
        }
        cmds.push_back(cmd);
    }

    return cmds;
}


std::vector<IOutput*> compute_expected(std::vector<ICmd*> cmds) {
    std::queue<Node> rs[FLOWS];
    std::priority_queue<Node, std::vector<Node>, Node> fs;
    int size[FLOWS] = {0};
    std::vector<IOutput*> out;
    
    int i = 0;
    for (ICmd* icmd : cmds) {
        Cmd* cmd = static_cast<Cmd*>(icmd);

        switch (cmd->op) {
            case Op::Push: {
                int idx = LOG(cmd->data.flow);
                if (size[idx] == SIZE) continue; 

                if (size[idx]) // push to rank store if flow non-empty
                    rs[idx].push(cmd->data);
                else {         // otherwise push to flow scheduler
                    cmd->data.cycle = i;
                    cmd->data.prio  = true;
                    fs.push(cmd->data);
                }
                size[idx]++;
                break;
            }

            case Op::Pop: {
                if (fs.empty()) continue;
                
                // pop flow scheduler
                Node n    = fs.top();
                int idx   = LOG(n.flow);
                Output* o = new Output;
                o->value  = n.value;
                out.push_back(o);
                fs.pop();
                size[idx]--;
                
                // update flow head from rank store
                if (size[idx]) {
                    n       = rs[idx].front();
                    n.cycle = i + 2;
                    n.prio  = false;
                    fs.push(n);
                    rs[idx].pop();
                }
                break;
            }

            case Op::Nop:
                // nothing to do...
                break;
        }

        i++;
    }
    
    return out;
}


std::vector<IOutput*> simulate(std::vector<ICmd*> cmds, const char* waveform) {
    if (!waveform) waveform = WAVEFORM;

    Vpifo *dut = new Vpifo;
    std::vector<IOutput*> out;

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
    std::vector<ICmd*>::iterator it = cmds.begin();
    int delay = 4; // pad with 4 cycles after all commands
    while (it != cmds.end() || delay) {
        dut->clk ^= 1;
        dut->eval();

        if (dut->clk) {
            if (dut->pop_valid) {
                Output* o = new Output;
                o->value = dut->pop_value;
                out.push_back(o);
            }

            if (it == cmds.end()) {
                dut->push = 0;
                dut->pop  = 0;
                delay--;
            }
            else {
                Cmd* cmd = static_cast<Cmd*>(*it);
                
                dut->push = cmd->op == Op::Push;
                dut->pop  = cmd->op == Op::Pop;

                dut->push_rank  = cmd->data.rank;
                dut->push_value = cmd->data.value;
                dut->push_flow  = cmd->data.flow;

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
