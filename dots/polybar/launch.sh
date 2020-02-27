#!/usr/bin/env bash

# Terminate already running bar instances
pkill polybar

export DEFAULT_NETWORK_INTERFACE=$(ls /sys/class/net | grep wlp | head -n 1 | awk '{print $1}')
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)

for m in $outputs; do
  export MONITOR=$m
  export TRAY_POSITION=right
  polybar --reload primary &
  disown
done
