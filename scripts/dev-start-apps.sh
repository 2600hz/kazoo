#!/bin/sh

cd $(dirname $0)

if [ -z "$1"]
then
  NODE_NAME=whistle_apps
else
  NODE_NAME=$1
fi

export ERL_CRASH_DUMP=$PWD/../$(date +%s)_apps_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$PWD/../deps:$PWD/../core:$PWD/../applications/

exec erl -name $NODE_NAME -args_file '/etc/kazoo/vm.args' -s reloader -s whistle_apps
