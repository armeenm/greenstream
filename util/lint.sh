#!/bin/sh
verilator -sv --lint-only common.sv top.sv uart.sv uart_rcv.sv uart_snd.sv
