#!/bin/sh
verilator -sv --lint-only top.sv uart.sv
