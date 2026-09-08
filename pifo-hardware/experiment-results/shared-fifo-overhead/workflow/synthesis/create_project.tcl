# quartus_sh -t create_project.tcl BUILD_DIR PART THREADS CLOCK_PERIOD_NS
package require ::quartus::project
load_package device
if {[llength $quartus(args)] < 4 || [llength $quartus(args)] > 5} {
    error {Expected BUILD_DIR PART THREADS CLOCK_PERIOD_NS [RTL_DIR]}
}
lassign $quartus(args) build_dir part threads period rtl_dir
set build_dir [file normalize $build_dir]
if {$rtl_dir eq ""} { set rtl_dir [file join $build_dir rtl] }
set rtl_dir [file normalize $rtl_dir]
set family [lindex [get_part_info -family $part] 0]
set rtl_files [glob -directory $rtl_dir *.v *.sv]
cd $build_dir
project_new pifo -overwrite
set_global_assignment -name FAMILY $family
set_global_assignment -name DEVICE $part
set_global_assignment -name TOP_LEVEL_ENTITY PifoMesh
set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files
set_global_assignment -name SEARCH_PATH $rtl_dir
set_global_assignment -name NUM_PARALLEL_PROCESSORS $threads
set_global_assignment -name OPTIMIZATION_MODE BALANCED
set_global_assignment -name SEED 1
foreach source $rtl_files {
    set_global_assignment -name SYSTEMVERILOG_FILE $source
}
set_global_assignment -name SDC_FILE pifo.sdc
# Standalone core resource measurement: no board pinout or NIC shell assumed.
set_instance_assignment -name VIRTUAL_PIN ON -to *
set_instance_assignment -name VIRTUAL_PIN OFF -to clk
set_instance_assignment -name VIRTUAL_PIN OFF -to reset
set sdc [open pifo.sdc w]
puts $sdc "create_clock -name clk -period $period \[get_ports clk\]"
puts $sdc {derive_clock_uncertainty}
puts $sdc {set_false_path -from [get_ports reset]}
close $sdc
export_assignments
project_close
puts "PIFO_DEVICE: $part ($family)"
