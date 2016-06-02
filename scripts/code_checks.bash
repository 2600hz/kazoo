#!/bin/bash

pushd $(dirname $0) > /dev/null

RAW_ROOT=$(pwd -P)/..
ROOT=$(realpath $RAW_ROOT)

function check_andalso_orelse {
    # check for andalso/orelse dropped lines
    BOOLS=$(grep -Elr '[a-zA-Z\)] (andalso|orelse)' $ROOT/{applications,core})

    if [ ${#BOOLS[@]} -ge 0 ]; then
        grep -Elr '[a-zA-Z\)] (andalso|orelse)' $ROOT/{applications,core}
    fi
}

check_andalso_orelse

popd > /dev/null
