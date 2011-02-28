#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

sname="whistle_apps"
[ ! -z "$1" ] && sname="$1"

exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin -pa $PWD/apps/*/ebin \
    -riak_err term_max_size 8192 fmt_max_bytes 9000 \
    -sasl errlog_type error \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -name $sname -s reloader -s whistle_apps
