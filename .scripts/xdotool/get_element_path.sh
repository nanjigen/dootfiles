# xdotool search "Mozilla Firefox" windowactivate --sync key --clearmodifiers ctrl+l
# xdotool search "Mozilla Firefox"
# xdotool search --onlyvisible --desktop 0 --classname "notepad.exe" windowfocus --sync
# xdotool search --onlyvisible --desktop 0 --classname "notepad.exe"
# xdotool search --onlyvisible --desktop 0 --classname "notepad.exe" windowactivate --sync type --delay 100 12345
# WID=`xdotool search --classname "notepad.exe" | head -1`
# xdotool windowactivate --sync $WID
# xdotool sleep 0.1
# xdotool type 12345
# xdotool key ctrl+c
xsel -bc
xdotool sleep 0.2
xdotool key shift+F10+f+c
# xdotool sleep 0.4
# xdotool key --delay 50 shift+F10+f+c
# xdotool sleep 1
xdotool sleep 0.2
xdotool key Escape
xclip -sel clip -o
# sleep 0.1
# xdotool key --clearmodifiers f c
# xdotool key --clearmodifiers c
# xdotool sleep 0.3
# xdotool key shift
# sleep 0.1
# xdotool sleep 1
# xdotool key f 
# xdotool key p
# sleep 0.2
# xdotool key f
# sleep 0.1
# xdotool key c
