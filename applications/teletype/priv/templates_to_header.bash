#!/bin/bash

pushd $(dirname $(dirname $0)) > /dev/null

HRL=src/teletype_default_modules.hrl
STARTED=0

echo "-ifndef(TELETYPE_DEFAULT_MODULES)." > $HRL

for TEMPLATE in $(find src/templates/ -name *.erl | sort); do
    FILENAME=$(basename $TEMPLATE)
    MODULE=${FILENAME%.erl}
    [[ $MODULE = 'teletype_template_skel' ]] && continue
    if [[ 0 -eq $STARTED ]]; then
        echo "-define(DEFAULT_MODULES, ['$MODULE'" >> $HRL
        STARTED=1
    else
        echo "                         ,'$MODULE'" >> $HRL
    fi
done

echo "                         ])." >> $HRL
echo "-define(TELETYPE_DEFAULT_MODULES, 'true')." >> $HRL
echo "-endif." >> $HRL

popd > /dev/null
