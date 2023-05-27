#!/usr/bin/env bash

# turn off passive cooling
systemctl stop tlp.service

# turn off active and passive scaling
sudo bash -c 'echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo'
sudo bash -c 'echo passive > /sys/devices/system/cpu/intel_pstate/status'

for x in /sys/devices/system/cpu/cpu*/cpufreq; do
    sudo bash -c "echo performance >> $x/scaling_governor"
    sudo bash -c "echo 1600000 >> $x/scaling_max_freq" # 1.6 GHz
    sudo bash -c "echo 1600000 >> $x/scaling_min_freq" # 1.6 GHz
done

grep --color=always . /sys/devices/system/cpu/intel_pstate/status
grep --color=always . /sys/devices/system/cpu/cpu*/cpufreq/{scaling_governor,scaling_max_freq,scaling_min_freq}

if ps aux | grep -q firefox; then
    echo 'Error: close firefox and other applications before benchmark'
    false
fi
