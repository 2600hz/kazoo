#!/bin/sh

cd `dirname $0`

if [ -z "$1" ]; then
    REMOTE_SHELL="ecallmgr@`hostname -f`"
else
    REMOTE_SHELL="$1"
fi

if [ -z "$2" ]; then
    SHELL_NAME="app_con_$(date +%s)@`hostname -f`"
else
    SHELL_NAME="$2"
fi

if [[ ! "$SHELL_NAME" == *@*  ]]; then
    SHELL_NAME="${SHELL_NAME}@`hostname -f`"
fi

if [[ ! "$REMOTE_SHELL" == *@*  ]]; then
    REMOTE_SHELL="${REMOTE_SHELL}@`hostname -f`"
fi

ERL_COOKIE=`../utils/sup/sup erlang get_cookie -n ecallmgr`

exec erl -setcookie $ERL_COOKIE -name ${SHELL_NAME} -remsh ${REMOTE_SHELL}
