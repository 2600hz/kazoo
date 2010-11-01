#!/bin/sh


cd `dirname $0`
exec erl -setcookie `cat ../fs_conf/autoload_configs/.erlang.cookie` -name ecallmgr_conn -remsh ecallmgr@`hostname -f`

