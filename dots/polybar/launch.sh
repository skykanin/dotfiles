#!/usr/bin/env bash

# Terminate already running bar instances
pkill polybar

network_interface=$(ls /sys/class/net | grep wlp | head -n 1 | awk '{print $1}')
export TRAY_POSITION="left"
export DEFAULT_NETWORK_INTERFACE="$network_interface"

# Monkey patch until real fix
# xrandr --output HDMI-0 --rotate left --right-of DP-2

# Wait for i3 to fully start..
# i3 isn't fully started when it execs this file, and polybar fails to connect
# to the i3 socket (or gets denied) and never shows up
sleep 0.2

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

outputs=$(xrandr --query | grep " connected" | cut -d " " -f 1)

for m in $outputs; do
  MONITOR=$m polybar --reload primary &> ~/polybar.error &
  disown
done
