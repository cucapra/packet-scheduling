# vivado -mode batch -source vivado_synth.tcl -tclargs BUILD_DIR PART THREADS CLOCK_PERIOD_NS PREPARE_ONLY [DIRECTIVE]
if {$argc < 5 || $argc > 6} {
    error {Expected BUILD_DIR PART THREADS CLOCK_PERIOD_NS PREPARE_ONLY [DIRECTIVE]}
}
lassign $argv build_dir part threads period prepare_only directive
if {$directive eq ""} { set directive default }
set build_dir [file normalize $build_dir]
set rtl_dir [file join $build_dir rtl]
set report_dir [file join $build_dir reports]
file mkdir $report_dir
if {[llength [get_parts -quiet $part]] != 1} {
    error "Target part is not installed: $part"
}
puts "PIFO_VIVADO_VERSION: [version -short]"
puts "PIFO_VIVADO_PART: $part"
set_param general.maxThreads $threads
create_project -in_memory -part $part
# Spinal emits memory initialization paths relative to the generated RTL.
cd $rtl_dir
read_verilog -sv [glob -directory $rtl_dir *.v *.sv]
set xdc [file join $build_dir pifo.xdc]
set constraints [open $xdc w]
puts $constraints "create_clock -name clk -period $period \[get_ports clk\]"
puts $constraints {set_false_path -from [get_ports reset]}
close $constraints
read_xdc $xdc
if {$prepare_only} {
    puts "PIFO_PREPARED: $build_dir"
    exit 0
}
synth_design -top PifoMesh -part $part -mode out_of_context -flatten_hierarchy rebuilt -directive $directive
write_checkpoint -force [file join $build_dir pifo_synth.dcp]
report_utilization -file [file join $report_dir utilization.rpt]
report_utilization -hierarchical -hierarchical_depth 5 -file [file join $report_dir utilization-hierarchy.rpt]
report_ram_utilization -file [file join $report_dir ram-utilization.rpt]
set blackboxes [get_cells -hierarchical -quiet -filter {IS_BLACKBOX == 1}]
if {[llength $blackboxes] != 0} {
    error "Unresolved blackboxes after synthesis: $blackboxes"
}
set primitive_counts [dict create]
foreach cell [get_cells -hierarchical -quiet -filter {IS_PRIMITIVE == 1}] {
    dict incr primitive_counts [get_property REF_NAME $cell]
}
set counts [open [file join $report_dir primitive-counts.tsv] w]
puts $counts "primitive\tcount"
foreach name [lsort [dict keys $primitive_counts]] {
    puts $counts "$name\t[dict get $primitive_counts $name]"
}
close $counts
set complete [open [file join $build_dir synthesis_complete.txt] w]
puts $complete "Vivado [version -short]; $part; PifoMesh; out_of_context; [clock format [clock seconds]]"
close $complete
puts "PIFO_SYNTHESIS_COMPLETE: $report_dir"
