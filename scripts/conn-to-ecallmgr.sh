#!/bin/sh

cd $(dirname $0)

./conn-to-apps.sh "ecallmgr@`hostname -f`"
