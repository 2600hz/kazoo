#!/bin/sh

cd `dirname $0`
exec erl -pa ebin/ -setcookie `cat ../fs_conf/autoload_configs/.erlang.cookie` -sname log_server -s log_server
