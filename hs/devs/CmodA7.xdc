## 12MHz Clock ##
set_property -dict {PACKAGE_PIN L17 IOSTANDARD LVCMOS33} [get_ports clk]
create_clock -name sys_clk_pin -period 83.33 -waveform {0 41.66} [get_ports {clk}];


## LEDs ##
set_property -dict {PACKAGE_PIN A17 IOSTANDARD LVCMOS33} [get_ports result_4]
set_property -dict {PACKAGE_PIN C16 IOSTANDARD LVCMOS33} [get_ports result_5]

#set_property -dict {PACKAGE_PIN B17 IOSTANDARD LVCMOS33} [get_ports nled_b]
#set_property -dict {PACKAGE_PIN B16 IOSTANDARD LVCMOS33} [get_ports nled_g]
#set_property -dict {PACKAGE_PIN C17 IOSTANDARD LVCMOS33} [get_ports nled_r]


## Buttons ##
set_property -dict {PACKAGE_PIN A18 IOSTANDARD LVCMOS33} [get_ports rst]
#set_property -dict {PACKAGE_PIN B18 IOSTANDARD LVCMOS33} [get_ports btn_start]


## GPIO Pins ##
set_property -dict {PACKAGE_PIN M3 IOSTANDARD LVCMOS33} [get_ports {result_3[16]}]
set_property -dict {PACKAGE_PIN L3 IOSTANDARD LVCMOS33} [get_ports {result_3[15]}]
set_property -dict {PACKAGE_PIN A16 IOSTANDARD LVCMOS33} [get_ports {result_3[12]}]
set_property -dict {PACKAGE_PIN K3 IOSTANDARD LVCMOS33} [get_ports {result_3[7]}]
set_property -dict {PACKAGE_PIN C15 IOSTANDARD LVCMOS33} [get_ports {result_3[6]}]
set_property -dict {PACKAGE_PIN H1 IOSTANDARD LVCMOS33} [get_ports {result_3[5]}]
set_property -dict {PACKAGE_PIN A15 IOSTANDARD LVCMOS33} [get_ports {result_3[4]}]
set_property -dict {PACKAGE_PIN B15 IOSTANDARD LVCMOS33} [get_ports {result_3[3]}]
set_property -dict {PACKAGE_PIN A14 IOSTANDARD LVCMOS33} [get_ports {result_3[2]}]
set_property -dict {PACKAGE_PIN J3 IOSTANDARD LVCMOS33} [get_ports {result_3[1]}]
set_property -dict {PACKAGE_PIN J1 IOSTANDARD LVCMOS33} [get_ports {result_3[0]}]
set_property -dict {PACKAGE_PIN K2 IOSTANDARD LVCMOS33} [get_ports {data_io[0]}]
set_property -dict {PACKAGE_PIN L1 IOSTANDARD LVCMOS33} [get_ports {data_io[1]}]
set_property -dict {PACKAGE_PIN L2 IOSTANDARD LVCMOS33} [get_ports {data_io[2]}]
set_property -dict {PACKAGE_PIN V3 IOSTANDARD LVCMOS33} [get_ports {data_io[3]}]
set_property -dict {PACKAGE_PIN W5 IOSTANDARD LVCMOS33} [get_ports {data_io[4]}]
set_property -dict {PACKAGE_PIN V4 IOSTANDARD LVCMOS33} [get_ports {data_io[5]}]
set_property -dict {PACKAGE_PIN U4 IOSTANDARD LVCMOS33} [get_ports {data_io[6]}]
set_property -dict {PACKAGE_PIN V5 IOSTANDARD LVCMOS33} [get_ports {data_io[7]}]
set_property -dict {PACKAGE_PIN W4 IOSTANDARD LVCMOS33} [get_ports result_0]
set_property -dict {PACKAGE_PIN U5 IOSTANDARD LVCMOS33} [get_ports {result_3[10]}]
set_property -dict {PACKAGE_PIN U2 IOSTANDARD LVCMOS33} [get_ports result_1]
set_property -dict {PACKAGE_PIN W6 IOSTANDARD LVCMOS33} [get_ports {result_3[11]}]
set_property -dict {PACKAGE_PIN U3 IOSTANDARD LVCMOS33} [get_ports {result_3[9]}]
set_property -dict {PACKAGE_PIN U7 IOSTANDARD LVCMOS33} [get_ports {result_3[8]}]
set_property -dict {PACKAGE_PIN W7 IOSTANDARD LVCMOS33} [get_ports {result_3[13]}]
set_property -dict {PACKAGE_PIN U8 IOSTANDARD LVCMOS33} [get_ports {result_3[14]}]
set_property -dict {PACKAGE_PIN V8 IOSTANDARD LVCMOS33} [get_ports result_2]


## UART ##
#set_property -dict {PACKAGE_PIN J18 IOSTANDARD LVCMOS33} [get_ports {rxd_out}];
set_property -dict {PACKAGE_PIN J17 IOSTANDARD LVCMOS33} [get_ports {rx}];


## Misc. ##
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
set_property CONFIG_MODE SPIx4 [current_design]
set_property BITSTREAM.CONFIG.CONFIGRATE 33 [current_design]
