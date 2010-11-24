#!/bin/sh

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

echo "Compiling applications"
cd ../applications/trunkstore
rebar clean compile
cd ../cdr
rebar clean compile
cd ../monitor
rebar clean compile
cd ../../bin
echo "Done compiling"
