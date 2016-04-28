#!/bin/sh

me="$(dirname $0)"

uniq -c | awk '{print $2, " ", $1}' | $me/flamegraph.pl
