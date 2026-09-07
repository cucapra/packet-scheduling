set root [file dirname [file normalize [info script]]]
lassign $argv mode
set_param general.maxThreads 2
if {$mode eq "estimate-only"} {
  set_param synth.elaboration.rodinMoreOptions {rename rt::check_resource rt::check_resource_device; proc rt::check_resource {args} {puts "RIO_ESTIMATION_ONLY_RESOURCE_CHECK_SKIPPED"; return}}
}
create_project -in_memory -part xcku5p-ffvb676-2-e
if {$mode eq "forced-block"} {
  read_verilog -sv [file join $root memory_probe_block.sv]
} else {
  read_verilog -sv [file join $root memory_probe.sv]
}
if {$mode eq "downgrade"} { set_msg_config -id {Synth 8-5834} -new_severity WARNING }
if {$mode eq "raised-limit"} {
  synth_design -top memory_probe -mode out_of_context -directive RuntimeOptimized -max_bram 200000
} else {
  synth_design -top memory_probe -mode out_of_context -directive RuntimeOptimized
}
report_utilization -file [file join $root $mode-utilization.rpt]
puts "RESOURCE_LIMIT_PROBE_COMPLETE $mode"
