#!/bin/sh

cd `dirname $0`
export ERL_CRASH_DUMP=$(date +%s)_ecallmgr_erl_crash.dump
export ERL_LIBS=$PWD/../deps:$PWD/../core
exec erl -name ecallmgr -args_file /etc/kazoo/vm.args -pa $PWD/../applications/*/ebin -pa $PWD/../core/*/ebin -pa $PWD/../deps/*/ebin -detached -s ecallmgr
