#!/bin/sh

cd amqp/
rebar clean compile
cd ../ecallmgr
rebar clean compile
cd ../diagnostics
rebar clean compile
cd ../applications/trunkstore
rebar clean compile
cd ../cdr
rebar clean compile
cd ../monitor
rebar clean compile
cd ../..
echo "Done compiling"
