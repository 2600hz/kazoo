#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

exec erl -detached -heart -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -name ecallmgr@`hostname -f` -s ecallmgr
