#include <vector>

#define LOG(x) (sizeof(unsigned int) * 8 - __builtin_clz(x) - 1)

enum class Op { 
    // for flow scheduler only
    Push1,    // single push through push port 1
    Push2,    // single push through push port 2
    PushPush, // dual push through both ports

    // for rank store only
    PushPop,

    Push,
    Pop, 
    Nop 
};

struct ICmd {
    virtual ~ICmd() = default;
    virtual std::string to_string() const = 0;
};

struct IOutput {
    virtual ~IOutput() = default;
    virtual std::string to_string() const = 0;
    virtual bool is_equal(const IOutput& other) const = 0;
};

std::vector<ICmd*>    generate_commands(int num_cmds, bool overflow);
std::vector<IOutput*> compute_expected(std::vector<ICmd*> cmds);
std::vector<IOutput*> simulate(std::vector<ICmd*> cmds, const char* waveform);
