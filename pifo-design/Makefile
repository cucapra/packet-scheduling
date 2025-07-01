TESTBENCHES=tb_flow_scheduler tb_rank_store

all: $(TESTBENCHES)

tb_%: src/%.sv tb/tb_%.cpp
	verilator --trace -cc src/$*.sv --exe tb/tb_$*.cpp --Mdir '.obj_dir'
	make -C .obj_dir -f V$*.mk V$*
	cp .obj_dir/V$* tb_$*

.PHONY: clean
clean:
	rm -rf .obj_dir
	rm -f *.vcd
	rm -f $(TESTBENCHES)
