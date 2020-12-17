#!/usr/bin/env bash 
if pgrep offlineimap; then pkill offlineimap; sleep 10s; fi 
offlineimap -o -u quiet
