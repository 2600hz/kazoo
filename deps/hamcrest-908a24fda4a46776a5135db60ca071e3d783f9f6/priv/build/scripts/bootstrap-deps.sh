#!/usr/bin/env sh

export PROPER_INSTALL_PATH=`epm info proper | awk '/install dir: .*/ { print $NF }'`
if [ "$PROPER_INSTALL_PATH" != "" ]; then
    mkdir -p deps
    ln -fs "$PROPER_INSTALL_PATH" deps/proper
fi