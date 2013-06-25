#!/bin/sh

cd `dirname $0`
export ERL_LIBS="../"
exec erl -setcookie `cat ../../confs/fs_conf/autoload_configs/.erlang.cookie` -pa $PWD/ebin -boot start_sasl -name wamqp -s whistle_amqp
