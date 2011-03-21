#!/bin/sh

cd `dirname $0`

if [ -z "$1" ]; then
    REMOTE_SHELL="whistle_apps@`hostname`"
else
    REMOTE_SHELL="$1"
fi

if [ -z "$2" ]; then
    SHELL_NAME="whistle_con_$(date +%s)@`hostname`"
else
    SHELL_NAME="$2"
fi

if [[ ! "$SHELL_NAME" == *@*  ]]; then
    SHELL_NAME="${SHELL_NAME}@`hostname`"
fi

if [[ ! "$REMOTE_SHELL" == *@*  ]]; then
    REMOTE_SHELL="${REMOTE_SHELL}@`hostname`"
fi

exec erl -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` -name ${SHELL_NAME} -remsh ${REMOTE_SHELL}
