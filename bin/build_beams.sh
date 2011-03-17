#!/bin/sh
WDIR="$PWD/`dirname $0`/../"

export ERL_LIBS=$WDIR/lib/

echo "Compiling utils"
cd $WDIR/utils/diagnostics
rebar clean compile

echo "Compiling ecallmgr"
cd $WDIR/ecallmgr
rebar clean compile

echo "Compiling Apps container and Apps"
cd $WDIR/applications
rebar clean compile

cd `readlink -f $0` # realpath
echo "Done compiling"
