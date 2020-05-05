#!/bin/bash
# ./check-git-status [directories]
# directories should be absolute paths

set -e

pushd "$(dirname "$0")" > /dev/null

ROOT=$(readlink -f "$(pwd -P)"/..)

function get_status {
    git_status=$(git -C $1 status --porcelain -uno)
    echo "$git_status"
}

statuses=""
for directory in $@; do
    if [ -d $directory ]; then
        dir_status=$(get_status "$directory")
        if [ -n "$dir_status" ]; then
            statuses+="$directory:\n$dir_status\n"
        fi
    fi
done

echo "$statuses"
