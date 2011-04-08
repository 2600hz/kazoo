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

fCleanUpLocal() {
    echo "# git gc --prune=now"
    git gc --prune=now
}

fCreateMissingBranches() {
    while read r; do
        l="`echo -n $r | cut -d/ -f 2`"
        if [[ -z "`git branch | grep $l`" && "$l" != "HEAD" ]]; then
            echo "# git branch --track $l $r"
            git branch --track $l $r
        fi
    done < <(git for-each-ref --format='%(refname:short)' refs/remotes)
}

cd `dirname $0`

fWelcome
fCleanUpLocal
fCreateMissingBranches

echo "Whistle remote branches imported"

exit 0
