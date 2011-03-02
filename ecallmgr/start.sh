#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib

exec erl -detached -heart -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -riak_err term_max_size 8192 fmt_max_bytes 9000 \
    -sasl errlog_type error \
    -sasl sasl_error_logger '{file, "log/error_log.sasl"}' \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -name ecallmgr@`hostname -f` -s reloader -s ecallmgr
