#!/bin/sh


cd `dirname $0`

sname="whistle_apps"
[ ! -z "$1" ] && sname="$1"

exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/apps/*/ebin \
    -sasl errlog_type error \
    -mnesia dir '"priv/mnesia"' \
    -boot start_sasl -name $sname -s whistle_apps
#    -kernel error_logger '{file, "log/error_log"}' \

