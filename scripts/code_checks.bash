#!/bin/bash

pushd $(dirname $0) > /dev/null

RAW_ROOT=$(pwd -P)/..
ROOT=$(realpath $RAW_ROOT)

function check_andalso_orelse {
    # check for andalso/orelse dropped lines
    BOOLS=$(grep -Elr '[a-zA-Z\)] (andalso|orelse)' $ROOT/{applications,core})

    if [ ${#BOOLS[@]} -ge 0 ]; then
        echo "check for andalso/orelse formatting issues in:"
        grep -Elr '[a-zA-Z\)] (andalso|orelse)' $ROOT/{applications,core}
    fi
}

function create_stub_schema {
    CF=$1
    SCHEMA=$2

    SKEL="$ROOT/applications/crossbar/priv/couchdb/schemas/callflows.skel.json"
    $(cp -a $SKEL $SCHEMA)
    $(sed -i "s/skel/$CF/g" $SCHEMA)
    echo "  created stub for $SCHEMA"
}

function check_callflow_schema {
    CF=$(basename $1)
    CF=${CF#cf_}
    CF=${CF%.erl}
    SCHEMA="$ROOT/applications/crossbar/priv/couchdb/schemas/callflows.$CF.json"

    if [ ! -f $SCHEMA ]; then
        create_stub_schema $CF $SCHEMA
    fi
}

function check_callflow_schemas {
    CALLFLOWS=$(ls $ROOT/applications/callflow/src/module/*.erl | grep -v skel)
    for CALLFLOW in $CALLFLOWS; do
        check_callflow_schema $CALLFLOW
    done
}

check_andalso_orelse

check_callflow_schemas

popd > /dev/null
