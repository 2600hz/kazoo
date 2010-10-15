#!/bin/sh


cd `dirname $0`
exec erl -setcookie ClueCon -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -name callmgr -s callmgr
