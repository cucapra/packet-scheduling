MODULE=flow_scheduler

.PHONY:build
build: obj_dir/V$(MODULE)

.PHONY:verilate
verilate: .stamp-verilate

./obj_dir/V$(MODULE): .stamp-verilate
	make -C obj_dir -f V$(MODULE).mk V$(MODULE)

.stamp-verilate: $(MODULE).sv tb.cpp
	verilator --trace -cc $(MODULE).sv --exe tb.cpp
	@touch .stamp-verilate

.PHONY: clean
clean:
	rm -rf .stamp-*;
	rm -rf ./obj_dir
	rm -rf *.vcd

