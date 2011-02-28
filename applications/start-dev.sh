#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

sname="whistle_apps"
[ ! -z "$1" ] && sname="$1"


exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/apps/*/ebin \
    -boot start_sasl -name $sname -s reloader -s whistle_apps
