#!/bin/sh

cd `dirname $0`

export ERL_LIBS=$PWD/../lib:$PWD/lib:$PWD/apps/*/lib
export ERL_CRASH_DUMP=$(date +%s)_erl_crash.dump

for path in $PWD/lib/*; do folder=$(basename $path); if [ "$folder" != "Makefile" ]; then echo "# rm -rf $PWD/../lib/$folder"; rm -rf $PWD/../lib/$folder; fi; done
exec erl -args_file $PWD/conf/vm.args -pa $PWD/deps/*/ebin $PWD/ebin $PWD/apps/*/ebin $PWD/apps/*/lib/*/ebin -detached -s whistle_apps
