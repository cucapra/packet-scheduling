exec p4c-vitisnet $::env(XILINX_VIVADO)/data/ip/xilinx/vitis_net_p4_v2_0/example_design/examples/five_tuple/fiveTuple.p4 -o cli_example.json

exec cp $::env(XILINX_VIVADO)/data/ip/xilinx/vitis_net_p4_v2_0/example_design/examples/five_tuple/traffic_in.user .

exec run-p4bm-vitisnet -p 9091 -s $::env(XILINX_VIVADO)/data/ip/xilinx/vitis_net_p4_v2_0/example_design/examples/five_tuple/cli_commands.txt -j cli_example.json

exec ls -l traffic_out.user traffic_out.meta