#!/bin/sh

cd `dirname $0`
exec erl -pa ebin/ -setcookie ClueCon -name log_server@`hostname` -s log_server
