#!/bin/bash

## dialyzes the changed erl files
## include extra beam files, like kz_json or gen_listener, as args to the script
## usage: ./dialyze-changed.bash [file.beam]

ERL_FILES=$(git --no-pager diff --name-only HEAD origin/master -- applications core | grep erl)

BEAM_FILES=()
BEAM=""

function erl_to_beam {
    APP_PATH=${1%%/src*}         ## core/APP or applications/APP
    BASENAME=${1##*/}            ## file.erl
    BEAM_FILE=${BASENAME/erl/beam} ## file.beam

    echo "$APP_PATH/ebin/$BEAM_FILE"
}

for ERL in $ERL_FILES; do
    BEAM=$(erl_to_beam $ERL)
    BEAM_FILES+=($BEAM)
done

ARGS=${BEAM_FILES[@]}
echo "dialyzing changed files:"
dialyzer --plt .kazoo.plt $MOD_BEAM $ARGS $@
