#!/bin/bash

## copies changed files (the beam code) to a host/path

pushd $(dirname $0) > /dev/null

cd $(pwd -P)/..

ERL_FILES=${ERL_FILES:-$(git --no-pager diff --name-only HEAD origin/master -- applications core | grep erl)}

HOST=${HOST:-"192.168.1.186"}
BEAM_PATH=${BEAM_PATH:-"/tmp/beams"}

function erl_to_beam {
    APP_PATH=${1%%/src*}         ## core/APP or applications/APP
    BASENAME=${1##*/}            ## file.erl
    BEAM_FILE=${BASENAME/erl/beam} ## file.beam

    echo "$APP_PATH/ebin/$BEAM_FILE"
}

[[ ! -z $ERL_FILES ]] && echo -n "syncing "
for ERL in $ERL_FILES; do
    BEAM=$(erl_to_beam $ERL)
    echo -n "."
    [ -f $BEAM ] && $(scp "$BEAM" "$HOST:$BEAM_PATH")
done
echo " done"

popd > /dev/null
