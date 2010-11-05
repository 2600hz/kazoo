#!/bin/sh

cd `dirname $0`
exec erl -setcookie `cat ../../fs_conf/autoload_configs/.erlang.cookie` -name ts_conn -remsh ts@`hostname -f`
