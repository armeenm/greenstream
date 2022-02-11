#!/usr/bin/env python3

import serial

with serial.Serial("/dev/ttyUSB3") as ser:
    for i in range(128):
        ser.write(8)
