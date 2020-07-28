#!/bin/sh

PIDS=`ps -ef | grep ipchain | grep -v grep`
if [ "$PIDS" = "" ]; then
    nohup ./ipchain -datadir="./ipc_data/ipchain" &
fi
