#!/bin/sh

PIDS=`ps -ef | grep Achain | grep -v grep`
if [ "$PIDS" = "" ]; then
    ./Achain --rpcuser game --rpcpassword xxx --httpdendpoint 172.16.10.157:8299 --server --daemon --data-dir ./act_data
fi
