#!/bin/sh
verilator -sv --lint-only top.sv uart.sv uart_rcv.sv uart_snd.sv
