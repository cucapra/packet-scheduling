create_clock -name clk -period 10.0 [get_ports clk]
derive_clock_uncertainty
set_false_path -from [get_ports reset]
