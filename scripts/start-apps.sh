#!/bin/sh

cd $(dirname $0)

export ERL_CRASH_DUMP=$PWD/../$(date +%s)_apps_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$PWD/../deps:$PWD/../core:$PWD/../applications/

exec erl -name kazoo_apps -args_file '/etc/kazoo/vm.args' -detached -s kazoo_apps
