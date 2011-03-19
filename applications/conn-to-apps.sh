#!/bin/sh

cd `dirname $0`

if [ -n "$1" ]; then
    REMOTE_SHELL="$1"
else
    REMOTE_SHELL="whistle_apps@`hostname`"
fi

if [ -n "$2" ]; then
    SHELL_NAME="$2"
else
    SHELL_NAME="app_con_$(date +%s)@`hostname`"
fi

exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` -name ${SHELL_NAME} -remsh ${REMOTE_SHELL}
