# xdotool search "Mozilla Firefox" windowactivate --sync key --clearmodifiers ctrl+l
# xdotool search "Mozilla Firefox"
# xdotool search --onlyvisible --desktop 0 --classname "notepad.exe" windowfocus --sync
# xdotool search --onlyvisible --desktop 0 --classname "notepad.exe"
# xdotool search --onlyvisible --desktop 0 --classname "notepad.exe" windowactivate --sync type --delay 100 12345
# WID=`xdotool search --classname "notepad.exe" | head -1`
# xdotool windowactivate --sync $WID
sleep 0.1
# xdotool type 12345
# xdotool key ctrl+c
xdotool key shift+F10
xdotool key --clearmodifiers f c
sleep 0.2
# xdotool key --clearmodifiers c 
# xdotool key p
# sleep 0.2
# xdotool key f
# sleep 0.1
# xdotool key c
