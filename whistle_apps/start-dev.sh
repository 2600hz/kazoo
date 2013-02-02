#!/bin/sh

cd `dirname $0`

export ERL_CRASH_DUMP=$(date +%s)_erl_crash.dump
export ERL_LIBS=$PWD/../lib:$PWD/lib

for path in $PWD/lib/*; do folder=$(basename $path); if [ "$folder" != "Makefile" ]; then echo "# rm -rf $PWD/../lib/$folder"; rm -rf $PWD/../lib/$folder; fi; done

echo $ERL_LIBS

exec erl -args_file $PWD/conf/vm.args -pa apps/*/ebin -pa ebin -s reloader -s whistle_apps
