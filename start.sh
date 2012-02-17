#!/bin/sh

tmux new-session -d -s bos

cd $HOME/bos/
tmux new-window -t bos:1 -n "webserver" "make start"
tmux new-window -t bos:2 -n "cert-daemon" "sh cert-daemon/cert-daemon-driver.sh"
tmux new-window -t bos:3 -n "callback-redir" "perl tools/wp-callpack-redirect.pl"
