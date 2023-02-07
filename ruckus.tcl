# Load RUCKUS library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Check for version 2020.1 of Vivado (or later)
if { [VersionCheck 2020.1] < 0 } {exit -1}

# Load ruckus files
loadRuckusTcl "$::DIR_PATH/core"
loadRuckusTcl "$::DIR_PATH/shared"

# Place and Route strategies
set_property strategy Performance_Explore [get_runs impl_1]

if { [info exists ::env(RELEASE)] != 1 } {
   puts "\n\nERROR: Release name not defined $::env(PROJ_DIR)/Makefile\n\n"; exit -1
}
