#!/bin/bash
# ./check-changed [directories]
# directories should be absolute paths

set -e

pushd "$(dirname "$0")" > /dev/null

ROOT=$(readlink -f "$(pwd -P)"/..)

function get_changed {
    diff=""
    for file in $(git -C $1 --no-pager diff --name-only HEAD); do
        diff+=" $ROOT/$file"
    done
    echo "$diff"
}

changed=""
for directory in $@; do
    if [ -d $directory ]; then
        dir_change=$(get_changed "$directory")
        changed+=" $dir_change"
    else
        echo "directory $directory not found"
    fi
done

echo "$changed"
