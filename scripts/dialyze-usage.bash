#!/bin/bash

## give a module name, grep the codebase for calls to that module
## create a list of beam files and feed it to dialyzer

pushd $(dirname $0) > /dev/null
cd $(pwd -P)/..

if [ -z ${ERL_FILES+x} ]; then
    MODULE=$1
    ERL_FILES=$(grep -rl "$1:" {core,applications} --include "*.erl" --exclude="\*pqc.erl" | grep -v "test/")
    MOD_BEAM=$(find {core,applications} -name "$MODULE.beam")
    echo "dialyzing usages of $MODULE"
    shift
fi

BEAM_FILES=()
BEAM=""

for ERL in $ERL_FILES; do
    APP_PATH=${ERL%%/src*}         ## core/APP or applications/APP
    BASENAME=${ERL##*/}            ## file.erl
    BEAM_FILE=${BASENAME/erl/beam} ## file.beam

    BEAM="$APP_PATH/ebin/$BEAM_FILE"
    BEAM_FILES+=($BEAM)
done

BEAM_FILES+=("core/kazoo_stdlib/ebin/kz_types.beam")
ARGS=${BEAM_FILES[@]}

dialyzer --plt .kazoo.plt $MOD_BEAM $ARGS $@

popd > /dev/null
