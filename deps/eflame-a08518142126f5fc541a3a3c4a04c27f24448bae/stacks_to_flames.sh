#!/bin/zsh

# This scripts renders separate flame graphs for each process in the file at the first argument
# Canvas sizes are tuned relatively to the longest running process (max is $maxwidth)

# usage:
# deps/eflame/stacks_to_flames.sh stacks.out

me="$(dirname $0)"
f=${1:-stacks.out}
maxwidth=${maxwidth:-1430}

for width pid in $(awk -F';' '{print $1}' $f | uniq -c | tr -d '<>' | sort -rn -k1); do
	max=${max:-$width.0}
	echo -n "pid: $pid\tsamples: $width\t"
	grep $pid $f | $me/flamegraph.pl --title="$title ($pid)" --width=$(($maxwidth / $max * $width)) > flame_$pid.svg
	echo flame_$pid.svg
done
