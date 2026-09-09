load_package device
foreach family {{Arria 10} {Stratix 10} {Agilex 7}} {
    puts "$family: [lrange [get_part_list -family $family] 0 5]"
}
puts "AGILEX_CANDIDATES: [lsearch -all -inline [get_part_list -family {Agilex 7}] AGFB014R24*]"
if {[llength $quartus(args)] > 0} {
    set part [lindex $quartus(args) 0]
    puts "TARGET_FAMILY: [get_part_info -family $part]"
}
