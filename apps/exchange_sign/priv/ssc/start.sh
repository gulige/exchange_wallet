#!/bin/sh

PIDS=`ps -ef | grep SSC-c | grep -v grep`
if [ "$PIDS" = "" ]; then
    ./SSC-c --rpcuser game --rpcpassword xxx --httpdendpoint 172.16.10.157:8080 --server --data-dir ./chain_data
fi
