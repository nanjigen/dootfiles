#!/bin/bash
clip=$(xclip -selection c -o)
line=$(sed -e 's#^z:##' -e 's#\\#/#g' <<< "$clip")
echo "$line"
