#!/bin/sh

erl -name local -setcookie `cat fs_conf/autoload_configs/.erlang.cookie`
