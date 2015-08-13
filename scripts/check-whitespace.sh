#!/bin/bash

echo "Files with trailing whitespace:"
# From /

ws="$(echo -ne ' \t\v')"

git grep -IE "[$ws]$" -- $* | cut -d: -f1 | sort -u && false || true
