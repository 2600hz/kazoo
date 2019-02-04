#!/bin/bash

set -e

pushd "$(dirname "$0")" >/dev/null

ROOT="$(pwd -P)"/..
cd $ROOT/core/kazoo_documents

errors=0
missing=""

for SRC in src/*.erl.src; do
    ERL=$(dirname $SRC)/$(basename $SRC .src)
    if [ ! -f "$ERL" ]; then
        missing="$missing $ERL - creating it from $SRC"$'\n'
        cp -a $SRC $ERL
        errors=1
    fi
done

popd >/dev/null

if [ $errors = 1 ]; then
    echo "The following accessor modules were auto-created:"
    echo "$missing"
    exit $errors
fi
