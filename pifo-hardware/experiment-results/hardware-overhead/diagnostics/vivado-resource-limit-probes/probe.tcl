set root [file dirname [file normalize [info script]]]
lassign $argv mode
set_param general.maxThreads 2
create_project -in_memory -part xcku5p-ffvb676-2-e
read_verilog -sv [file join $root memory_probe.sv]
if {$mode eq "downgrade"} { set_msg_config -id {Synth 8-5834} -new_severity WARNING }
if {$mode eq "raised-limit"} {
  synth_design -top memory_probe -mode out_of_context -directive RuntimeOptimized -max_bram 2000
} else {
  synth_design -top memory_probe -mode out_of_context -directive RuntimeOptimized
}
report_utilization -file [file join $root $mode-utilization.rpt]
puts "RESOURCE_LIMIT_PROBE_COMPLETE $mode"
