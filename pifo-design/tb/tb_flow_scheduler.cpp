#include <queue>
#include <vector>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vflow_scheduler.h"

#include "tb.hpp"

#define FLOWS 10
#define SIZE  10

#define WAVEFORM "flow_scheduler.vcd"

struct Node {
    unsigned int rank;
    unsigned int flow;
    unsigned int value;
    int counter; // to break ties in FIFO order 

    bool operator()(const Node& l, const Node& r) {
        if (l.rank == r.rank) 
            return l.counter > r.counter;

        return l.rank > r.rank;
    }
};

struct Output : public IOutput {
    unsigned int flow;
    unsigned int value;

    std::string to_string() const {
        char buff[50];
        snprintf(buff, sizeof(buff), "v=%u\tf=%u", value, flow);
        return buff;
    }

    bool is_equal(const IOutput& other) const {
        const Output& o = static_cast<const Output&>(other);
        return value == o.value && flow == o.flow;
    }
};

struct Cmd : public ICmd {
    Op op;
    Node data_1;
    Node data_2;

    std::string to_string() const {
        unsigned int v_1 = data_1.value, 
                     r_1 = data_1.rank, 
                     f_1 = data_1.flow;
        unsigned int v_2 = data_2.value, 
                     r_2 = data_2.rank, 
                     f_2 = data_2.flow;

        char buff[100];
        switch (op) {
            case Op::Push1:
                snprintf(buff, sizeof(buff), "push(v=%u, f=%u, r=%u) (port 1)", 
                         v_1, f_1, r_1);
                break;

            case Op::Push2:
                snprintf(buff, sizeof(buff), "push(v=%u, f=%u, r=%u) (port 2)", 
                         v_2, f_2, r_2);
                break;

            case Op::Pop:
                snprintf(buff, sizeof(buff), "pop");
                break;

            case Op::PushPush:
                snprintf(buff, sizeof(buff), 
                         "push(v=%u, f=%u, r=%u) + push(v=%u, f=%u, r=%u)",
                         v_1, f_1, r_1, v_2, f_2, r_2);
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
    int size = 0;
    
    for (int i = 0; i < num_cmds; i++) {
        Cmd* cmd = new Cmd;

        if (!overflow && size == SIZE) // to avoid overflow
            cmd->op = Op::Pop;
        else if (!overflow && !size)   // to avoid underflow
            cmd->op = rand() % 2 ? Op::Push1 : Op::Push2;
        else {
            switch (rand() % 5) {
                case 0:
                    cmd->op = Op::Push1;
                    break;
                case 1:
                    cmd->op = Op::Push2;
                    break;
                case 2:
                    cmd->op = Op::Pop;
                    break;
                case 3:
                              // to avoid overflow
                    cmd->op = !overflow && size == SIZE - 1 ? Op::Push1 : Op::PushPush;
                    break;
                case 4:
                    cmd->op = Op::Nop;
                    break;
            }
        }

        unsigned int v_1 = rand() % 1000, 
                     r_1 = rand() % 1000, 
                     f_1 = 1U << (rand() % FLOWS);
        unsigned int v_2 = rand() % 1000, 
                     r_2 = rand() % 1000, 
                     f_2 = 1U << (rand() % FLOWS);

        cmd->data_1 = {r_1, f_1, v_1, 2 * i};
        cmd->data_2 = {r_2, f_2, v_2, 2 * i + 1};
        cmds.push_back(cmd);

        int delta;
        switch (cmd->op) {
            case Op::Push1:    
            case Op::Push2:    delta = 1;  break;
            case Op::Pop:      delta = -1; break;
            case Op::PushPush: delta = 2;  break;
            case Op::Nop:      delta = 0;  break;
        }
        size = size + delta;
    }
    
    // flush
    while (size > 0) {
        Cmd* cmd = new Cmd;
        cmd->op  = Op::Pop;
        cmds.push_back(cmd);
        size--;
    }

    return cmds;
}


std::vector<IOutput*> compute_expected(std::vector<ICmd*> cmds) {
    std::priority_queue<Node, std::vector<Node>, Node> pifo;
    std::vector<IOutput*> out;

    for (ICmd* icmd : cmds) {
        Cmd* cmd = static_cast<Cmd*>(icmd);

        switch (cmd->op) {
            case Op::Pop: {
                if (pifo.empty()) continue;

                Node    n = pifo.top();
                Output* o = new Output;
                o->flow   = n.flow;
                o->value  = n.value;
                out.push_back(o);
                pifo.pop();
                break;
            }

            case Op::PushPush:
                // fall through

            case Op::Push1:
                if (pifo.size() == SIZE) continue;

                pifo.push(cmd->data_1);
                if (cmd->op == Op::Push1) break;

            case Op::Push2:
                if (pifo.size() == SIZE) continue;

                pifo.push(cmd->data_2);
                break;

            case Op::Nop:
                // nothing to do...
                break;
        }
    }
    
    return out;
}


std::vector<IOutput*> simulate(std::vector<ICmd*> cmds, const char* waveform) {
    if (!waveform) waveform = WAVEFORM;

    Vflow_scheduler *dut = new Vflow_scheduler;
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
                o->flow  = dut->pop_flow;
                o->value = dut->pop_value;
                out.push_back(o);
            }

            if (it == cmds.end()) {
                dut->push_1 = 0;
                dut->push_2 = 0;
                dut->pop = 0;
                delay--;
            }
            else {
                Cmd* cmd = static_cast<Cmd*>(*it);
                
                dut->push_1 = cmd->op == Op::Push1 || cmd->op == Op::PushPush;
                dut->push_2 = cmd->op == Op::Push2 || cmd->op == Op::PushPush;
                dut->pop    = cmd->op == Op::Pop;

                dut->push_rank_1  = cmd->data_1.rank;
                dut->push_value_1 = cmd->data_1.value;
                dut->push_flow_1  = cmd->data_1.flow;
                dut->push_rank_2  = cmd->data_2.rank;
                dut->push_value_2 = cmd->data_2.value;
                dut->push_flow_2  = cmd->data_2.flow;

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
