#!/bin/sh

cd `dirname $0`

export ERL_LIBS=$PWD/../
exec erl -args_file $PWD/conf/vm.args -pa $PWD/ebin -s reloader -s whistle_couch
