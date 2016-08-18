#!/bin/bash

# Usage:
#
# ./dev-exec-mfa.sh module_name [start_fun [arg1 arg2 .... argN]]
#

DIR=$(dirname $0)

export ERL_CRASH_DUMP=$DIR/../$(date +%s)_apps_erl_crash.dump
export ERL_LIBS="$ERL_LIBS":$DIR/../deps:$DIR/../core:$DIR/../applications/

MODULE=$1
FUN=$2
ARGS=${@:3}

erl -name $MODULE -s compile file $MODULE \
                  -s code load_abs $MODULE \
                  -eval "'ok' = kz_util:ensure_started('sasl')." \
                  -eval "'ok' = kz_util:ensure_started('crypto')." \
                  -eval "'ok' = kz_util:ensure_started('gproc')." \
                  -eval "'ok' = kz_util:ensure_started('lager')." \
                  -eval "'ok' = kz_util:ensure_started('kazoo_caches')." \
                  -eval "'ok' = kz_util:ensure_started('kazoo_token_buckets')." \
                  -eval "'ok' = kz_util:ensure_started('kazoo_amqp')." \
                  -eval "'ok' = kz_util:ensure_started('kazoo_couch')." \
                  -s $MODULE $FUN $ARGS \
                  -s erlang halt

rm ${MODULE}.beam

