#!/bin/bash

set -e

pushd "$(dirname "$0")" >/dev/null

ROOT=$(readlink -f "$(pwd -P)"/..)

errors=0
missing=""

for SRC in $ROOT/core/kazoo_documents/src/*.erl.src; do
    ERL=$(dirname $SRC)/$(basename $SRC .src)
    if [ ! -f "$ERL" ]; then
        missing="$missing $ERL - creating it from $SRC"$'\n'
        cp -a $SRC $ERL
        errors=1
    fi
done

if [ $errors = 1 ]; then
    echo "The following accessor modules were auto-created:"
    echo "$missing"

    popd >/dev/null

    exit $errors
fi

missing=""

for SRC_DIFF in $(git --no-pager diff --name-only HEAD -- $ROOT/core/kazoo_documents/src/*.src); do
    ERL=$(dirname $SRC_DIFF)/$(basename $SRC_DIFF .src)
    ERL_DIFF=$(git --no-pager diff --name-only HEAD -- $ERL)
    if [ -z $ERL_DIFF ]; then
        missing="$missing $ERL:1: "$'\n'
        errors=1
    fi
done

if [ $errors = 1 ]; then
    echo "The following .erl files are missing changes from their .src:"
    echo $missing
    popd >/dev/null
    exit $errors
fi

popd >/dev/null
