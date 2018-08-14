#!/bin/sh

cd $(dirname $0)

ROOT=$PWD/..

export ERL_CRASH_DUMP=$ROOT/$(date +%s)_ecallmgr_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$ROOT/deps:$ROOT/core:$ROOT/applications:$(echo $ROOT/deps/rabbitmq_erlang_client-*/deps)

NODE_NAME=${1:-ecallmgr}

export KAZOO_APPS=ecallmgr

exec erl \
     -name $NODE_NAME \
     -args_file $ROOT/rel/dev.vm.args \
     -config $ROOT/rel/sys.config \
     -s reloader
