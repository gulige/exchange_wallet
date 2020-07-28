#!/bin/sh
echo "starting wallet node......"
ERL_FLAGS=" -args_file config/vm_wallet.args -config config/sys_wallet.config" rebar3 shell --apps exchange_wallet
