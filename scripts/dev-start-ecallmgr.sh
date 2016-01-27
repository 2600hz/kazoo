#!/bin/sh

cd $(dirname $0)

ROOT=$PWD/..

NODE_NAME=ecallmgr

export ERL_CRASH_DUMP=$ROOT/$(date +%s)_ecallmgr_erl_crash.dump

# Note: 'reloader' isn't started automatically
# Note: the 'vm.args' file used is 'rel/vm.args'

RELX_REPLACE_OS_VARS=true KZname=$NODE_NAME $ROOT/_rel/kazoo/bin/kazoo 'console' "$$@"
