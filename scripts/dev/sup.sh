#!/bin/bash
: ${KAZOO_NODE=kazoo_apps@$(hostname)}
echo Connecting to node: $KAZOO_NODE

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
