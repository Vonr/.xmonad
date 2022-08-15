#!/bin/sh

chosen=$(printf "  Lock\n  Logout\n  Restart\n  Power Off" | rofi -dmenu -i -l 4)

case "$chosen" in
    "  Lock") xtrlock ;;
    "  Logout") pkill -f xmonad ;;
	"  Restart") reboot ;;
	"  Power Off") poweroff ;;
	*) exit 1 ;;
esac
