action=im_change_whole_desktop
input_method=mozc
printf "%s\n%s\n\n" "$action" "$input_method" | \
    nc -NU "$XDG_RUNTIME_DIR/uim/socket/uim-helper"
