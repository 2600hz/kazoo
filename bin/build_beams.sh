#!/bin/sh
WDIR="$PWD/`dirname $0`/.."
REBAR=$WDIR/bin/rebar

export ERL_LIBS=$WDIR/lib/


echo "Compiling utils"
cd $WDIR/utils/diagnostics
$REBAR clean compile

echo "Compiling Whistle LIBS"
for WLIB in $WDIR/lib/whistle*
do 
    cd $WLIB
    $REBAR clean compile 
done

echo "Compiling ecallmgr"
cd $WDIR/ecallmgr
$REBAR clean compile

echo "Compiling Apps container and Apps"
cd $WDIR/applications
$REBAR clean compile

cd `readlink -f $0` # realpath
echo "Done compiling"
