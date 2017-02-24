#!/bin/bash

pushd $(dirname $0) > /dev/null

exit_signal=0

missing_descriptions=$(egrep -l "\"description\":\\s?\"\"" ../applications/crossbar/priv/couchdb/schemas/*.json)
if [ $? -eq 0 ]; then
    echo "JSON schemas missing property descriptions:"
    for missing in $missing_descriptions; do
        echo "  " $(basename $missing)
    done
    exit_signal=1
fi

popd > /dev/null

exit $exit_signal
