#! /bin/sh

DIRS=.\ `ls -l | grep '^d' | awk '{print $8}'`

for d in $DIRS; do
    rm -f $d/*~ $d/#*# $d/*.dump
done
