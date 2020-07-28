#!/bin/sh

PIDS=`ps -ef | grep wicc | grep -v grep`
if [ "$PIDS" = "" ]; then
    tar -xzf wicc.tar.gz
    chmod +x wicc
    ./wicc -datadir=cur -server -daemon
fi
