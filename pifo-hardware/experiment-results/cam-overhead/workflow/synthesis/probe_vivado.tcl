# vivado -mode batch -source probe_vivado.tcl -tclargs OUTPUT_TSV
if {$argc != 1} {
    error {Expected OUTPUT_TSV}
}
set output [open [lindex $argv 0] w]
puts $output "part\tdevice\tfamily"
set devices [dict create]
foreach part [get_parts -quiet] {
    set device [get_property DEVICE $part]
    set family [get_property FAMILY $part]
    dict set devices $device $family
    puts $output "$part\t$device\t$family"
}
close $output
puts "Vivado [version -short] installed devices:"
dict for {device family} $devices {
    puts "$device ($family)"
}
