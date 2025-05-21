# Make sure you have run the nix build command to build the formal results
# Now the result/ directory should contain the HIA.sv and HIAFormal.sv

clear -all

# Analyze design under verification files
set RESULT_PATH .

# Analyze source files and property files
analyze -sv12 -f ${RESULT_PATH}/filelist.f

# Elaborate design and properties
elaborate -top HIAFormal

# Set up Clocks and Resets
clock clock
reset reset

# Get design information to check general complexity
get_design_info

# Prove properties
# 1st pass: Quick validation of properties with default engines
set_max_trace_length 100
prove -all

report -file report.txt

set failed_properties [get_property_list -include {status {cex unreachable}}]
set length [llength $failed_properties]
if { $length > 0 } {
  puts "There are $length failed properties!"
  exit 1
} else {
  puts "All properties passed!"
  exit 0
}
