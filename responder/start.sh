#!/bin/sh


cd `dirname $0`
exec erl -detached -heart -setcookie ClueCon -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -name responders
