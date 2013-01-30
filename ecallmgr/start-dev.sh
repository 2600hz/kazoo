#!/bin/sh

cd `dirname $0`

export ERL_CRASH_DUMP=$(date +%s)_erl_crash.dump
exec erl -args_file $PWD/conf/vm.args -pa ../lib ebin -s reloader -s ecallmgr
