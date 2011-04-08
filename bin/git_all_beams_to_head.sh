#!/bin/bash

fWelcome() {
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

fCheckoutBeam() {
    echo "# git checkout HEAD $1"
    
    git checkout HEAD $1

    return $?
}

fConfirm() {
    read -p "This will revert all beams to the HEAD versions. Are you sure you want to do this(y/N)? " -n 1

    for confirm in y Y yes YES Yes; do
        [ "${REPLY}" == "${confirm}" ] && echo && return 0
    done

    echo
    return 1
}

cd `dirname $0`

fWelcome
fConfirm || exit 1

while read beam; do
    fCheckoutBeam $beam || exit 1
done < <(find ../ -name "*.beam")

echo "All erlang beam files reverted to HEAD versions."

exit 0
