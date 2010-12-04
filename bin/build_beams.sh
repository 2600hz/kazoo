#!/bin/sh

cd `dirname $0`

echo "Compiling utils"
cd ../utils/amqp/
rebar clean compile
cd ../couch
rebar clean compile
cd ../diagnostics
rebar clean compile

echo "Compiling ecallmgr"
cd ../../ecallmgr
rebar clean compile

echo "Compiling Apps container"
cd ../applications
rebar clean compile

echo "Compiling applications"
cd apps/trunkstore
rebar clean compile
cd ../cdr
rebar clean compile
cd ../monitor
rebar clean compile
cd ../hangups
rebar clean compile

cd `readlink -f $0` # realpath
echo "Done compiling"
