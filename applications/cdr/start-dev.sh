#!/bin/sh


cd `dirname $0`
exec erl -setcookie `cat ../../fs_conf/autoload_configs/.erlang.cookie`  -pa $PWD/ebin \
    -boot start_sasl -sname cdr -s cdr
#    -kernel error_logger '{file, "log/error_log"}' \

