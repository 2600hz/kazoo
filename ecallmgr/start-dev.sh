#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -boot start_sasl -name ecallmgr@`hostname` -s ecallmgr
#    -kernel error_logger '{file, "log/error_log"}' \

