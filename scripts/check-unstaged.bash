#!/bin/bash

root=$(readlink -f "$(dirname $0)"/..)

changed=$($root/scripts/check-git-status.bash "$root" "$root/core" "$root/applications/*")

if [ -n "$changed" ]; then
    echo Unstaged changes!
    echo -e "$changed"
    exit 1
fi
