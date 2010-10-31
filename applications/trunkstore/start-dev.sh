#!/bin/sh


cd `dirname $0`
exec erl -setcookie ClueCon -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -boot start_sasl -sname ts -s trunkstore
#    -kernel error_logger '{file, "log/error_log"}' \

