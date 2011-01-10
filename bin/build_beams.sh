#!/bin/sh

cd `dirname $0`
export ERL_LIBS=$PWD/../lib/

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

echo "Compiling Apps container and Apps"
cd ../applications
rebar clean compile

cd `readlink -f $0` # realpath
echo "Done compiling"
