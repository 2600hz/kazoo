#!/bin/sh

cd $(dirname $0)

ROOT=$PWD/..

export ERL_CRASH_DUMP=$ROOT/$(date +%s)_ecallmgr_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$ROOT/deps:$ROOT/core:$ROOT/applications:$(echo $ROOT/deps/rabbitmq_erlang_client-*/deps)

if [ -z "$1"]
then
  NODE_NAME=whistle_apps
else
  NODE_NAME=$1
fi

exec erl -name $NODE_NAME -args_file $ROOT/rel/dev-vm.args -s reloader
