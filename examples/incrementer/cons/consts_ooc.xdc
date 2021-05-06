set clockPeriod 2

create_clock -period $clockPeriod [get_ports clk]
set_property HD.CLK_SRC BUFGCTRL_X0Y0 [get_ports clk]

#set_input_delay -clock clk 1 [get_ports -filter { NAME !~ "*clk*" DIRECTION == "IN" }]
#set_output_delay -clock clk [expr $clockPeriod * 0.9] [get_ports -filter { NAME =~  "*" && DIRECTION == "OUT" }]
