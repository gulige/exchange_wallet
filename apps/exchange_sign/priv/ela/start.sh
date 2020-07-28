#!/bin/sh

PIDS=`ps -ef | grep Elastos.ELA.Utilities.Java.jar | grep -v grep`
if [ "$PIDS" = "" ]; then
    java -cp Elastos.ELA.Utilities.Java.jar org.elastos.elaweb.HttpServer &
fi
