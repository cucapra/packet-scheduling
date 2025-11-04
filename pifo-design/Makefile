TESTBENCHES=tb_pifo tb_rank_store tb_flow_scheduler

all: $(TESTBENCHES)

tb_%: src/%.sv tb/main.cpp tb/tb_%.cpp
	verilator --trace -cc src/$*.sv \
		--exe tb/main.cpp tb/tb_$*.cpp \
		--Mdir '.obj_dir' -y src/
	make -C .obj_dir -f V$*.mk V$*
	cp .obj_dir/V$* tb_$*

.PHONY: clean
clean:
	rm -rf .obj_dir
	rm -f *.vcd
	rm -f $(TESTBENCHES)
