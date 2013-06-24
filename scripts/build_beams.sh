#!/bin/sh

WDIR="$PWD/`dirname $0`/.."
REBAR=$WDIR/bin/rebar

fWelcome() {                                                                                                                                                                                                                                                                                                                              [0/1842]
    clear
    echo "======================================================="
    echo " _  _  _ _     _ _____    _    _______ _       _______ "
    echo "| || || | |   | (_____)  | |  (_______) |     (_______)"
    echo "| || || | |__ | |  _      \ \  _      | |      _____   "
    echo "| ||_|| |  __)| | | |      \ \| |     | |     |  ___)  "
    echo "| |___| | |   | |_| |_ _____) ) |_____| |_____| |_____ "
    echo " \______|_|   |_(_____|______/ \______)_______)_______)"
    echo " - - - Signaling the start of next generation telephony"
    echo "======================================================="
    echo
}

fExportLibs() {
    echo "# export ERL_LIBS=${WDIR}/lib/"
    export ERL_LIBS=${WDIR}/lib/
}

fWelcome

fExportLibs

cd ${WDIR}
make clean
make

exit 0
