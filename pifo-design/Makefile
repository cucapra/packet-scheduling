MODULES=./obj_dir/Vflow_scheduler ./obj_dir/Vrank_store

all: $(MODULES)

./obj_dir/V%: %.sv tb_%.cpp
	verilator --trace -cc $*.sv --exe tb_$*.cpp
	make -C obj_dir -f V$*.mk V$*

.PHONY: clean
clean:
	rm -rf .stamp-*;
	rm -rf ./obj_dir
	rm -rf *.vcd

