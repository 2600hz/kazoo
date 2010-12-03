#!/bin/sh

cd `dirname $0`
exec erl -setcookie `cat ../../fs_conf/autoload_configs/.erlang.cookie` -name cdr_conn -remsh cdr@`hostname -f`
