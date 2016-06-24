#!/bin/bash
KAZOO_NODE=${KAZOO_NODE:-kazoo_apps@$(hostname)}
echo Connecting to node: $KAZOO_NODE

IFS='@' read -r -a NH <<< "$KAZOO_NODE"
NAME="${NH[0]}"
HOST="${NH[1]}"

RELX_REPLACE_OS_VARS=true KZname="-name $KAZOO_NODE" \
	exec _rel/kazoo/bin/kazoo escript lib/sup-*/priv/sup.escript \
	-n $NAME \
	-h $HOST \
	"$*"
