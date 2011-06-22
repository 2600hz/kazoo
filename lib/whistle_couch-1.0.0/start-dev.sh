#!/bin/sh

cd `dirname $0`
export ERL_LIBS="../"

exec erl -pa $PWD/ebin -boot start_sasl -sname couch -s whistle_couch
