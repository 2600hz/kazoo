#!/bin/sh


cd `dirname $0`
exec erl -detached -heart -setcookie `cat ../fs_conf/autoload_configs/.erlang.cookie` -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -name ecallmgr -s ecallmgr

