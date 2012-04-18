#!/bin/sh

cd `dirname $0`

export ERL_LIBS=$PWD/../../lib
exec erl -args_file $PWD/conf/vm.args -pa $PWD/deps/*/ebin $PWD/ebin -s lineman
