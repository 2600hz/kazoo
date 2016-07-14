#!/bin/bash

pushd $(dirname $0) > /dev/null

RAW_ROOT=$(pwd -P)/../..
CF_ROOT=$(realpath $RAW_ROOT)

function create_stub_schema {
    CF=$1
    SCHEMA=$2

    SKEL="$CF_ROOT/../crossbar/priv/couchdb/schemas/callflows.skel.json"
    $(cp -a $SKEL $SCHEMA)
    $(sed -i "s/skel/$CF/g" $SCHEMA)
    echo "  created stub for $SCHEMA"
}

function check_callflow_schema {
    CF=$(basename $1)
    CF=${CF#cf_}
    CF=${CF%.erl}
    SCHEMA="$CF_ROOT/../crossbar/priv/couchdb/schemas/callflows.$CF.json"

    if [ ! -f $SCHEMA ]; then
        create_stub_schema $CF $SCHEMA
    fi
}

function check_callflow_schemas {
    CALLFLOWS=$(ls $CF_ROOT/src/module/*.erl | grep -v skel)
    for CALLFLOW in $CALLFLOWS; do
        check_callflow_schema $CALLFLOW
    done
}

check_callflow_schemas

popd > /dev/null
