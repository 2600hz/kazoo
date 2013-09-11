#!/bin/sh

cd `dirname $0`

export ERL_CRASH_DUMP=$(date +%s)_apps_erl_crash.dump
export ERL_LIBS=$PWD/../deps:$PWD/../core
exec erl -name whistle_apps -args_file /etc/kazoo/vm.args -pa $PWD/../applications/*/ebin -pa $PWD/../core/*/ebin -pa $PWD/../deps/*/ebin $PWD/../deps/*/ebin -pa $PWD/../applications/*/lib/*/ebin -s reloader -s whistle_apps
