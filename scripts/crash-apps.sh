#!/bin/bash

cd $(dirname $0)

NODE="kazoo_apps"
HOSTNAME=$(/bin/hostname)
REMOTE_SHELL="${NODE}@${HOSTNAME}"
SUP="../core/sup/priv/sup"

if [ -z "${1}" ]; then
    ERL_COOKIE=$(${SUP} -n ${NODE} erlang get_cookie | sed "s/'//g")
else
    ERL_COOKIE="${1}"
fi

erl_call -c ${ERL_COOKIE} -name ${REMOTE_SHELL} -a 'erlang halt ["admin-request"]'
