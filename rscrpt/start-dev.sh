#!/bin/sh


cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -sname rscrpt -s rscrpt
