#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

sname="whistle_apps"
[ ! -z "$1" ] && sname="$1"


exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/apps/*/ebin \
    -mnesia dir '"priv/mnesia"' \
    -mnesia debug true \
    -boot start_sasl -name $sname -s reloader -s whistle_apps
#     -sasl errlog_type error \
#    -kernel error_logger '{file, "log/error_log"}' \
