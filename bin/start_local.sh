#!/bin/sh

erl -name local@`hostname -f` -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie`
