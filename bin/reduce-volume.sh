#!/usr/bin/env sh
pacat -r -d alsa_input.pci-0000_00_1b.0.analog-stereo --latency=1msec | sox -b 16 -e signed -c 2 -r 44100 -t raw - -b 16 -e signed -c 2 -r 44100 -t raw - noisered ~/lib/noise.prof 0.2 | pacat -p -d alsa_output.pci-0000_00_1b.0.analog-stereo --latency=1msec
