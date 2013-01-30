#!/bin/sh

cd `dirname $0`

export ERL_LIBS=$PWD/../lib
export ERL_CRASH_DUMP=$(date +%s)_erl_crash.dump
exec erl -args_file $PWD/conf/vm.args -pa ../lib -pa ebin -heart -detached -s ecallmgr
