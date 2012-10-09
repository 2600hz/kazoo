#!/bin/sh

cd `dirname $0`

export ERL_LIBS=$PWD/../lib
export ERL_CRASH_DUMP=$(date +%s)_erl_crash.dump
exec erl -args_file $PWD/conf/vm.args -pa $PWD/deps/*/ebin $PWD/ebin $PWD/apps/*/ebin -s reloader -s whistle_apps
