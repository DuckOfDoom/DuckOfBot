#!/bin/sh
set -e
tmux new-session -d 'ghcid'
tmux split-window -v 'while true; do ./start.sh; sleep 1; done'
tmux split-window -h 'stack build --file-watch --exec "killall duckbot"'
tmux attach-session -d
