#!/usr/bin/env bash

action=im_change_whole_desktop
input_method=mozc
printf "%s\n%s\n\n" "$action" "$input_method" | \
    "~/.uim.d/socket/uim-helper"
