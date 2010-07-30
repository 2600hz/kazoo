#!/bin/sh


cd `dirname $0`
exec erl -heart -detached -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -sname rscrpt -s rscrpt
