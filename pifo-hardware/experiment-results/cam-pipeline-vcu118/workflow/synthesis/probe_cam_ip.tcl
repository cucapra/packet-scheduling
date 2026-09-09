# Catalog/license inspection only. No synth_design, synth_ip or implementation.
# vivado -mode batch -source synthesis/probe_cam_ip.tcl -tclargs /tmp/rio-cam-probe
if {$argc < 1 || $argc > 2} {
    error "usage: probe_cam_ip.tcl OUTPUT_DIRECTORY ?PART?"
}
set outdir [file normalize [lindex $argv 0]]
set part xcvu9p-flga2104-2L-e
if {$argc == 2} { set part [lindex $argv 1] }
file mkdir $outdir
file mkdir [file join $outdir ip]
create_project -in_memory -part $part
set defs [get_ipdefs -all -quiet *:cam:*]
set info [open [file join $outdir catalog.txt] w]
puts $info "part=$part"
puts $info "definitions=$defs"
foreach def $defs {
    foreach prop {DISPLAY_NAME LICENSE_KEYS REQUIRES_LICENSE} {
        puts $info "$def $prop=[get_property $prop $def]"
    }
}
close $info
if {[llength $defs] == 0} { error "No CAM IP found in this installation" }
create_ip -vlnv [lindex [lsort -dictionary $defs] end] -module_name rio_cam_probe -dir [file join $outdir ip]
report_ip_status -file [file join $outdir ip-status.rpt]
puts "RIO_CAM_CATALOG_PROBE_COMPLETE: [file join $outdir ip-status.rpt]"
exit
