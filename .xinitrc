setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'

xset r rate 300 25

dunst &
# dwmblocks &
# exec dwm
# exec i3
exec openbox-session
