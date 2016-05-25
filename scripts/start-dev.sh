#!/bin/sh
# this script should be run from kazoo repository clone root

if [ "$KAZOO_CONFIG" = "" ]
then
	export KAZOO_CONFIG=$PWD/config.ini
fi

if [ "$KAZOO_NODE" = "" ]
then
	KAZOO_NODE=kazoo@localhost
fi

CMD=$1
if [ "$CMD" = "" ]
then
	CMD=console
fi

# some modules get compiled to this path by sync
export ERL_LIBS=_rel/kazoo

make build-dev-release

RELX_REPLACE_OS_VARS=true KZname="-name $KAZOO_NODE" _rel/kazoo/bin/kazoo $CMD
