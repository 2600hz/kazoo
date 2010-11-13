#!/bin/sh


cd `dirname $0`
exec erl -setcookie `cat ../fs_conf/autoload_configs/.erlang.cookie` -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -boot start_sasl -name ecallmgr@`hostname -f` -s ecallmgr
#    -kernel error_logger '{file, "log/error_log"}' \

