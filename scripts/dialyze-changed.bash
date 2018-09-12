#!/bin/bash

## dialyzes the changed erl files
## include extra beam files, like kz_json or gen_listener, as args to the script
## usage: ./dialyze-changed.bash [file.beam]

pushd $(dirname $0) > /dev/null
cd $(pwd -P)/..

ERL_FILES=$(git --no-pager diff --name-only HEAD origin/4.3 -- applications core | grep erl)

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
    if [ -f $BEAM ]; then
        BEAM_FILES+=($BEAM)
    fi
done

BEAM_FILES+=("core/kazoo_stdlib/ebin/kz_types.beam")
ARGS=${BEAM_FILES[@]}
echo "dialyzing changed files(${#BEAM_FILES[@]}):"
dialyzer --plt .kazoo.plt $MOD_BEAM $ARGS $@

popd > /dev/null
