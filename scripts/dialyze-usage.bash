#!/bin/bash

## give a module name, grep the codebase for calls to that module
## create a list of beam files and feed it to dialyzer

ERL_FILES=$(grep -rl "$1:" {core,applications} --include "*.erl" --exclude="\*pqc.erl" | grep -v "test/")
MOD_BEAM=$(find {core,applications} -name "$1.beam")

BEAM_FILES=()
BEAM=""

for ERL in $ERL_FILES; do
    APP_PATH=${ERL%%/src*}         ## core/APP or applications/APP
    BASENAME=${ERL##*/}            ## file.erl
    BEAM_FILE=${BASENAME/erl/beam} ## file.beam

    BEAM="$APP_PATH/ebin/$BEAM_FILE"
    BEAM_FILES+=($BEAM)
done

ARGS=${BEAM_FILES[@]}
echo "dialyzing usages of $1"
dialyzer --plt .kazoo.plt $MOD_BEAM $ARGS
