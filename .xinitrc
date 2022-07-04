trayer --edge top --align right --SetPartialStrut true --expand true --width 5 --transparent true --alpha 0 --tint 0x1d2021 --height 22 &

dunst &

redshift -PO 3750

feh --bg-scale $HOME/Pictures/wallpapers/mathematical_symbol_fight_2x.png

xbanish &

exec dbus-launch xmonad
