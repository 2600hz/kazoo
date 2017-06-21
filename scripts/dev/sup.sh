#!/bin/bash
KAZOO_NODE=${KAZOO_NODE:-sup@$(hostname)}
echo Connecting to node: $KAZOO_NODE

RELX_REPLACE_OS_VARS=true RELX_MULTI_NODE=true KZname="$KAZOO_NODE" _rel/kazoo/bin/kazoo escript lib/sup-*/priv/sup.escript "$*"
