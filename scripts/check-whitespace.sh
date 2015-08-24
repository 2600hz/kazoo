#!/bin/bash

echo "Files with trailing whitespace:"
# From /

ws="$(echo -ne ' \t\v')"

count=0
for file in $(git grep -IE "[$ws]$" -- $* | cut -d: -f1 | sort -u); do
    echo "$file"
    ((count++))
done

exit $count
