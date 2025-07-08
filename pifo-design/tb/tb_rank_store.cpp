#include <queue>
#include <vector>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vrank_store.h"

#include "tb.hpp"

#define FLOWS 10
#define SIZE  50

#define WAVEFORM "rank_store.vcd"

struct Output : public IOutput {
    unsigned int rank;
    unsigned int value;

    std::string to_string() const {
        char buff[50];
        snprintf(buff, sizeof(buff), "v=%u\tr=%u", value, rank);
        return buff;
    }

    bool is_equal(const IOutput& other) const {
        const Output& o = static_cast<const Output&>(other);
        return value == o.value && rank == o.rank;
    }
};

struct Cmd : public ICmd {
    Op op;
    Output rank_value;
    unsigned int push_flow;
    unsigned int pop_flow;

    std::string to_string() const {
        unsigned int v = rank_value.value, r = rank_value.rank;

        char buff[100];
        switch (op) {
            case Op::Push:
                snprintf(buff, sizeof(buff), "push(v=%u, f=%u, r=%u)", v, push_flow, r);
                break;

            case Op::Pop:
                snprintf(buff, sizeof(buff), "pop(f=%u)", pop_flow);
                break;

            case Op::PushPop:
                snprintf(buff, sizeof(buff), "push(v=%u, f=%u, r=%u) + pop(f=%u)", 
                         v, push_flow, r, pop_flow);
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
    int size[FLOWS] = {0};

    for (int i = 0; i < num_cmds; i++) {
        Cmd* cmd = new Cmd;

        unsigned int v         = rand() % 1000, 
                     r         = rand() % 1000, 
                     push_flow = rand() % FLOWS,
                     pop_flow  = rand() % FLOWS;

        if (!overflow && size[push_flow] == SIZE) { // to avoid overflow
            pop_flow = push_flow;
            cmd->op = Op::Pop;
        }
        else if (!overflow && !size[pop_flow]) {    // to avoid underflow
            push_flow = pop_flow;
            cmd->op = Op::Push;
        }
        else {
            switch (rand() % 4) {
                case 0: cmd->op = Op::Push;    break;
                case 1: cmd->op = Op::Pop;     break;
                case 2: 
                    cmd->op = Op::PushPop; 
                    // bias odds toward concurrent push-pops on same flow
                    if (rand() % 2) push_flow = pop_flow;
                    break;
                case 3: cmd->op = Op::Nop;     break;
            }
        }

        cmd->rank_value.rank  = r;
        cmd->rank_value.value = v;
        cmd->push_flow        = 1U << push_flow;
        cmd->pop_flow         = 1U << pop_flow;
        cmds.push_back(cmd);
        
        switch (cmd->op) {
            case Op::Push: size[push_flow]++; break;
            case Op::Pop:  size[pop_flow]--;  break;
            case Op::Nop:                     break;
            case Op::PushPop: 
                    size[push_flow]++;
                    size[pop_flow]--;
                    break;
        }
    }
    
    // flush all flows
    for (unsigned int f = 0; f < FLOWS; f++)
        while (size[f] > 0) {
            Cmd* cmd      = new Cmd;
            cmd->op       = Op::Pop;
            cmd->pop_flow = 1U << f;
            cmds.push_back(cmd);
            size[f]--;
        }

    return cmds;
}


std::vector<IOutput*> compute_expected(std::vector<ICmd*> cmds) {
    std::queue<Output> bank[FLOWS];
    std::vector<IOutput*> out;

    for (ICmd* icmd : cmds) {
        Cmd* cmd = static_cast<Cmd*>(icmd);

        int pop_idx  = LOG(cmd->pop_flow);
        int push_idx = LOG(cmd->push_flow);

        switch (cmd->op) {
            case Op::PushPop:
                // fall through

            case Op::Push: {
                if (bank[push_idx].size() == SIZE) continue;

                bank[push_idx].push(cmd->rank_value);
                if (cmd->op == Op::Push) break;
            }

            case Op::Pop: {
                if (bank[pop_idx].empty()) continue;

                Output  n = bank[pop_idx].front();
                Output* o = new Output;
                o->rank   = n.rank;
                o->value  = n.value;
                out.push_back(o);
                bank[pop_idx].pop();
                break;
            }

            case Op::Nop:
                // nothing to do...
                break;
        }
    }
    
    return out;
}


std::vector<IOutput*> simulate(std::vector<ICmd*> cmds, const char* waveform) {
    if (!waveform) waveform = WAVEFORM;

    Vrank_store *dut = new Vrank_store;
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
                o->rank = dut->pop_rank;
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
                
                dut->push = cmd->op == Op::PushPop || cmd->op == Op::Push;
                dut->pop  = cmd->op == Op::PushPop || cmd->op == Op::Pop;
                
                dut->push_value = cmd->rank_value.value;
                dut->push_rank  = cmd->rank_value.rank;
                dut->push_flow  = cmd->push_flow;
                dut->pop_flow   = cmd->pop_flow;

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
