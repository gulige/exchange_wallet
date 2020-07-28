#!/bin/sh
echo "starting signature node......"
ERL_FLAGS=" -args_file config/vm_sign.args -config config/sys_sign.config" rebar3 shell --apps exchange_sign
