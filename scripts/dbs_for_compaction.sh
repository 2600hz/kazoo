#!/bin/sh

SIZE="100M"

ssh $1 "find /srv/db/shards/ -size +$SIZE -exec ls -s {} \;" | sort -n | awk -F "/" '{printf "%s/%s/%s/%s\n", $6,$7,$8,$9}' | awk -F "." '{printf "%s\n", $1}' | uniq

