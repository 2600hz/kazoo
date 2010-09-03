#!/bin/sh


cd `dirname $0`
exec erl -setcookie ClueCon -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -sname ecallmgr -s ecallmgr
