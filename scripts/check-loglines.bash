#!/bin/bash

set -e

pushd "$(dirname "$0")" >/dev/null

ROOT=$(readlink -f "$(pwd -P)"/..)

errors=0
erls=""

for ERL in $(egrep -rl "lager:\w+\(\"[A-Z][a-z]" $ROOT/core $ROOT/applications); do
    sed -i 's/lager:\w+("\([A-Z]\)[a-z]/\L\1/' $ERL
    errors=1
    erls="$erls$ERL:1: log lines starting with capital letters"$'\n'
done

if [ $errors = 1 ]; then
    echo "$erls"
fi

popd >/dev/null

exit $errors
