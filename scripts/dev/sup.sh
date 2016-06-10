#!/bin/bash
if [ "$KAZOO_CONFIG" = "" ]
then
	export KAZOO_CONFIG=$PWD/config.ini
fi

if [ "$KAZOO_NODE" = "" ]
then
	KAZOO_NODE=kazoo@$(hostname)
fi

IFS='@' read -r -a NH <<< "$KAZOO_NODE"
NAME="${NH[0]}"
HOST="${NH[1]}"

# request can execute longer than 5 seconds, so timeout 10 is required

RELX_REPLACE_OS_VARS=true KZname="-name $KAZOO_NODE" \
	_rel/kazoo/bin/kazoo escript lib/sup-*/priv/sup.escript \
	-n $NAME \
	-h $HOST \
	-c change_me \
	-t 10 "$*"
