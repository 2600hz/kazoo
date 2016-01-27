#!/bin/sh

cd $(dirname $0)

ROOT=$PWD/..

if [ -z "$1"]
then
  NODE_NAME=whistle_apps
else
  NODE_NAME=$1
fi

export ERL_CRASH_DUMP=$ROOT/$(date +%s)_apps_erl_crash.dump

# Note: 'reloader' isn't started automatically
# Note: the 'vm.args' file used is 'rel/vm.args'

RELX_REPLACE_OS_VARS=true KZname=$NODE_NAME $ROOT/_rel/kazoo/bin/kazoo 'console' "$$@"
