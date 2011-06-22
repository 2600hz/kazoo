#!/bin/bash

USERNAME=$(git config user.name)
EMAIL=$(git config user.email)
LEVEL="global"
CLEVEL="global"

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

fGetUsername() {
    USERNAME=$(git config ${CLEVEL}user.name)
    read -p "Enter the name to use on git commits [$USERNAME]: " RESP
    if [ ! -z "$RESP" ]; then
        USERNAME="$RESP"
    fi
}

fGetEmail() {
    EMAIL=$(git config ${CLEVEL}user.email)
    read -p "Enter the email to use on git commits [$EMAIL]: " RESP
    if [ ! -z "$RESP" ]; then
        EMAIL="$RESP"
    fi
}

fConfigLevel() {
    while true; do
        read -p "What level config do you want to change system, global, or repository? (s/g/r)[$LEVEL]:" -n 1 RESP
        if [ -z "$RESP" ]; then
            break
        fi
        echo
        case $RESP in
            [Ss]* ) LEVEL="system";break;;
            [Gg]* ) LEVEL="global";break;;
            [Rr]* ) LEVEL="repository";break;;            
            * ) clear;fWelcome;;
        esac
    done
    if [ "$LEVEL" = "repository" ]; then
        CLEVEL=""
    else
        CLEVEL="--$LEVEL "
    fi
}

fGetEditor() {
    read -p "Enter the editor to use for git commit messages [$EMAIL]: " RESP
    if [ -z "$USERNAME" ]; then
        EDITOR="$(whoami)"
    else
        EDITOR="$RESP"
    fi
}

fSetupUser() {
    echo "# git config ${CLEVEL}user.name \"$USERNAME\""
    git config ${CLEVEL}user.name "$USERNAME"

    echo "# git config ${CLEVEL}user.email $EMAIL"
    git config ${CLEVEL}user.email "$EMAIL"
}

fEnableRerere() {
    echo "# git config ${CLEVEL}rerere.enabled 1"
    git config ${CLEVEL}rerere.enabled 1
}

fEnableColor() {
    echo "# git config ${CLEVEL}color.ui true"
    git config ${CLEVEL}color.ui true
}

fLineEndingPrefs() {
  echo "# git config ${CLEVEL}core.autocrlf input"
  git config ${CLEVEL}core.autocrlf input
  # git config --global core.autocrlf true ## Windows

  echo "# git config ${CLEVEL}core.safecrlf true"
  git config ${CLEVEL}core.safecrlf true
}

cd `dirname $0`

clear

fWelcome
fConfigLevel
fGetUsername
fGetEmail

fSetupUser
fEnableRerere
fEnableColor
fLineEndingPrefs

echo "Git configuration complete."

exit 0
