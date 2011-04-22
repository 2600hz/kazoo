#!/bin/sh

cd `dirname $0`

if [ -z "$1" ]; then
    REMOTE_SHELL="ecallmgr@`hostname`"
else
    REMOTE_SHELL="$1"
fi

if [ -z "$2" ]; then
    SHELL_NAME="app_con_$(date +%s)@`hostname`"
else
    SHELL_NAME="$2"
fi

if [[ ! "$SHELL_NAME" == *@*  ]]; then
    SHELL_NAME="${SHELL_NAME}@`hostname`"
fi

if [[ ! "$REMOTE_SHELL" == *@*  ]]; then
    REMOTE_SHELL="${REMOTE_SHELL}@`hostname`"
fi

ERL_COOKIE=`grep "setcookie" conf/vm.args`

exec erl $ERL_COOKIE -name ${SHELL_NAME} -remsh ${REMOTE_SHELL}
