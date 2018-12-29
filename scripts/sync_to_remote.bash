#!/bin/bash

## copies changed files (the beam code) to a host/path

## Usage:
## ./sync_to_remote.bash
##   Syncs all files that diff from origni/master
## ERL_FILES=$(git diff --name-only origin/{BRANCH} *.erl) HOST="{SERVER}" ./scripts/sync_to_remote.bash
##   Sync any unpushed changes to HOST

pushd $(dirname $0) > /dev/null

cd $(pwd -P)/..

ERL_FILES=${ERL_FILES:-$(git --no-pager diff --name-only HEAD origin/4.3 -- applications core | grep erl)}

HOST=${HOST:-"192.168.1.186"}
PORT=${PORT:-"22"}
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
    [ -f $BEAM ] && $(scp -P$PORT "$BEAM" "$HOST:$BEAM_PATH")
done
echo " done"

popd > /dev/null
