#!/bin/sh

cd $(dirname $0)

# DEFAULT_ROOT="/home/hesaam/work/2600hz/kazoo/_rel/kazoo"
ROOT=$PWD/..

export ERL_CRASH_DUMP=$ROOT/$(date +%s)_ecallmgr_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$ROOT/deps:$ROOT/core:$ROOT/applications:$(echo $ROOT/deps/rabbitmq_erlang_client-*/deps)

NODE_NAME=${1:-ecallmgr}

export KAZOO_APPS=ecallmgr

exec erl \
     -name $NODE_NAME \
     -args_file $ROOT/rel/dev-vm.args \
     -config $ROOT/rel/sys.config \
     -s reloader

# RELX_REPLACE_OS_VARS=true RELX_MULTI_NODE=true KZname="${NODE_NAME}" "${DEFAULT_ROOT}"/bin/kazoo console -s reloader
