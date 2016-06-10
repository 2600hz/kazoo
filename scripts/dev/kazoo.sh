#!/bin/bash

if [ "$KAZOO_CONFIG" = "" ]
then
	export KAZOO_CONFIG=$HOME/config.ini
	echo Using default config: $KAZOO_CONFIG
fi

if [ ! -f $KAZOO_CONFIG ]; then
	echo "Specified kazoo config doesn't exists, please provide readable kazoo config file"
	exit
fi

if [ "$KAZOO_NODE" = "" ]
then
	KAZOO_NODE=kazoo@$(hostname)
	echo Using default node name $KAZOO_NODE
fi

CMD=$1
if [ "$CMD" = "" ]
then
	CMD=console
else
	shift
fi

RELX_REPLACE_OS_VARS=true KZname="-name $KAZOO_NODE" _rel/kazoo/bin/kazoo $CMD "$*"
