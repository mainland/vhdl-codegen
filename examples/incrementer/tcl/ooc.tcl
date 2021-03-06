#
# See:
#   https://forums.xilinx.com/t5/Timing-Analysis/Measuring-the-timing-bottleneck-of-an-isolated-component-without/m-p/1232349
#   https://forums.xilinx.com/t5/Implementation/How-to-synthesize-implement-a-part-of-the-design-out-of-context/td-p/823023
#

set outputDir ./output
file mkdir $outputDir
set_part xc7vx485tffg1761-2

# STEP#1: setup design sources and constraints
read_vhdl -vhdl2008 rtl/incrementer.vhd
set_property top main [current_fileset]
set_property top_file {rtl/incrementer.vhd} [current_fileset]
read_xdc cons/consts_ooc.xdc

# STEP#2: run synthesis, report utilization and timing estimates, write checkpoint design
synth_design -top main -mode out_of_context
write_checkpoint -force $outputDir/post_synth
report_utilization -file $outputDir/post_synth_util.rpt
report_timing -sort_by group -max_paths 5 -path_type summary \
  -file $outputDir/post_synth_timing.rpt

# STEP#3: run placement and logic optimization, report utilization and timing estimates
opt_design -directive Explore
power_opt_design
place_design -directive Explore
phys_opt_design
write_checkpoint -force $outputDir/post_place
report_clock_utilization -file $outputDir/clock_util.rpt
report_utilization -file $outputDir/post_place_util.rpt
report_timing -sort_by group -max_paths 5 -path_type summary \
  -file $outputDir/post_place_timing.rpt

# STEP#4: run router, report actual utilization and timing, write checkpoint design, run DRCs
route_design
write_checkpoint -force $outputDir/post_route
report_timing_summary -file $outputDir/post_route_timing_summary.rpt
report_utilization -file $outputDir/post_route_util.rpt
report_power -file $outputDir/post_route_power.rpt
report_methodology -file $outputDir/post_impl_checks.rpt
report_drc -file $outputDir/post_imp_drc.rpt
write_verilog -force $outputDir/impl_netlist.v
write_xdc -no_fixed_only -force $outputDir/impl.xdc
