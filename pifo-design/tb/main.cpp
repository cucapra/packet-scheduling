#include <iostream>
#include <cstring> 
#include "tb.hpp"

#define NUM_CMDS 50
#define SUCCESS  "\x1B[32mTEST PASS\033[0m\t\t"
#define FAILURE  "\x1B[31mTEST FAIL\033[0m\t\t" 


int main(int argc, char** argv, char** env) {
    int  num_cmds = NUM_CMDS;
    bool verbose  = false;
    bool overflow = true;
    const char* waveform = nullptr;

    for (int i = 0; i < argc; i++) {
        if ( !strcmp(argv[i], "-v") ) 
            verbose = true;
        if ( !strcmp(argv[i], "--no-overflow") )
            overflow = false;
        if ( !strcmp(argv[i], "-n") && i + 1 < argc )
            num_cmds = atoi(argv[i + 1]);
        if ( !strcmp(argv[i], "-w") && i + 1 < argc )
            waveform = argv[i + 1];
        if ( !strcmp(argv[i], "-h") ) {
            std::cout << argv[0] << " [-h] [-v] [-n NUM_CMDS] [-w FILE.vcd]" << std::endl;
            return 0;
        }
    }

    std::vector<ICmd*>    cmds   = generate_commands(num_cmds, overflow);
    std::vector<IOutput*> expect = compute_expected(cmds);
    std::vector<IOutput*> output = simulate(cmds, waveform);
    
    if (verbose) {
        std::cout << "Commands" << std::endl;
        for (ICmd* c : cmds) std::cout << c->to_string() << std::endl;

        std::cout << "Expected" << std::endl;
        for (IOutput* x : expect) std::cout << x->to_string() << std::endl;

        std::cout << "Output" << std::endl;
        for (IOutput* x : output) std::cout << x->to_string() << std::endl;
    }

    for (ICmd* c : cmds) delete c;
    
    bool equal = true;
    for (int i = 0; i < expect.size() || i < output.size(); i++) {
        IOutput* e = i < expect.size() ? expect[i] : nullptr;
        IOutput* o = i < output.size() ? output[i] : nullptr;
        equal &= e && o ? e->is_equal(*o) : false;
        
        if (e) delete e;
        if (o) delete o;
    }

    if (equal)
        std::cout << SUCCESS << std::endl;
    else
        std::cout << FAILURE << std::endl;
    
    return !equal;
}
