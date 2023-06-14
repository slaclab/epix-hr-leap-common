# Load RUCKUS library
source -quiet $::env(RUCKUS_DIR)/vivado_proc.tcl

# Load local Source Code
loadSource -dir "$::DIR_PATH/rtl"

loadSource -path "$::DIR_PATH/ip/SysMonCore.dcp"

if { $::env(RELEASE) == "EPixHR10k2M" } {

   # Adding the default Si5345 configuration
    add_files -norecurse "$::DIR_PATH/pll-config/EPixHR10k2M/EPixHR10k2MPllConfigClk5EnClk3-2.5V-Registers.mem"
    puts "Using EPixHR10k2M PLL configuration"

} elseif { $::env(RELEASE) == "ePixHRM320k" } {

    add_files -norecurse "$::DIR_PATH/pll-config/ePix320KM/EPixHRM320KPllConfigClk5EnClk32.5V-Registers.mem"
    puts "Using ePixHRM320k PLL configuration"
} else {
    puts "No pll configuration was used!!"
}
