#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

if [ -z $1 ]; then
    SHELL_NAME="whistle_apps@`hostname`"
else
    SHELL_NAME="$1"
fi

if [[ ! "$SHELL_NAME" == *@*  ]]; then
    SHELL_NAME="${SHELL_NAME}@`hostname`"
fi

exec erl -detached -heart -setcookie `cat ../confs/fs_conf/autoload_configs/.erlang.cookie` \
    -pa $PWD/ebin -pa $PWD/apps/*/ebin \
    -riak_err term_max_size 8192 fmt_max_bytes 9000 \
    -sasl errlog_type error \
    -sasl sasl_error_logger '{file, "log/error_log.sasl"}' \
    -kernel error_logger '{file, "log/error_log"}' \
    -boot start_sasl -name ${SHELL_NAME} -s reloader -s whistle_apps
