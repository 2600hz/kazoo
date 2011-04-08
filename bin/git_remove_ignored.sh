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

fFindIgnoredFiles() {
    while read file; do
        fGitRm ${file}
    done < <(find ../$1 2> /dev/null)
}

fGitRm() {
    git ls-files --error-unmatch "$1" 2> /dev/null || return 0

    echo "# git rm --cached $1"
    git rm --cached $1
}

cd `dirname $0`

fWelcome

while read ignore; do
    fFindIgnoredFiles ${ignore}
done < ../.gitignore

echo "All ignored files removed from git, please commit any changes now."
