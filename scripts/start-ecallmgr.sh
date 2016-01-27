#!/bin/sh

cd $(dirname $0)

export ERL_CRASH_DUMP=$PWD/../$(date +%s)_ecallmgr_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$PWD/../deps:$PWD/../core:$PWD/../applications/

exec erl -name 'ecallmgr' -args_file '/etc/kazoo/vm.args' -detached -s ecallmgr
