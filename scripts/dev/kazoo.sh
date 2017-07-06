#!/bin/sh

# look for kazoo release root directory
DEFAULT_ROOT=${KAZOO_ROOT:-_rel/kazoo}
if [ -d "$DEFAULT_ROOT/_rel/kazoo" ]; then
    DEFAULT_ROOT="$DEFAULT_ROOT"
elif [ -d "$DEFAULT_ROOT/bin" ]; then
    DEFAULT_ROOT="$DEFAULT_ROOT"
elif [ -d /opt/kazoo/_rel/kazoo ]; then
    DEFAULT_ROOT="/opt/kazoo/_rel/kazoo"
elif [ -d /opt/kazoo/bin ]; then
    DEFAULT_ROOT="/opt/kazoo"
else
    echo "Can't find Kazoo release root directory, is the release built?"
    exit -1
fi
echo "Release path: $DEFAULT_ROOT"

# look for kazoo config file path
if [ -f "$KAZOO_CONFIG" ]; then
    KAZOO_CONFIG="$KAZOO_CONFIG"
elif [ -f $HOME/config.ini ]; then
    KAZOO_CONFIG=$HOME/config.ini
elif [ -f /etc/kazoo/config.ini ]; then
    KAZOO_CONFIG="/etc/kazoo/config.ini"
elif [ -f /etc/kazoo/core/config.ini ]; then
    KAZOO_CONFIG="/etc/kazoo/core/config.ini"
else
    echo "Kazoo config doesn't exists, please provide readable kazoo config file"
    exit -1
fi
echo "Kazoo config file path: $KAZOO_CONFIG"

NODE_NAME=${NODE_NAME:-kazoo_apps@$(hostname -f)}
echo "Node name: $NODE_NAME"

COOKIE=${COOKIE:-"change_me"}
echo "Cookie: $COOKIE"

CMD=$1
if [ "$CMD" = "" ]; then
    CMD=console
else
    shift
fi

RELX_REPLACE_OS_VARS=true RELX_MULTI_NODE=true NODE_NAME="$NODE_NAME" COOKIE="$COOKIE" "$DEFAULT_ROOT"/bin/kazoo $CMD "$*"
