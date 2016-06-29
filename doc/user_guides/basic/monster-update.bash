#!/bin/bash

pushd `dirname $0` > /dev/null

APPS_PATH="monster_apps"
REPOS=(accounts
       callflows
       numbers
       pbxs
       voip
       webhooks
      )

function update_repo {
    echo "  updating to latest $1"
    pushd $1 > /dev/null
    $(git fetch --quiet origin)
    $(git rebase --quiet origin/master)
    $(git gc --quiet)
    popd > /dev/null
    echo "  updated to latest $1"
}

function setup_repo {
    if [ ! -d $1 ]; then
        echo "  cloning $1"
        $(git clone https://github.com/2600hz/monster-ui-$1 $1)
    else
        update_repo $1
    fi
}

function sync_to_monster {
    pushd $APPS_PATH > /dev/null
    for app in $(ls -d *); do
        if [[ ! $app == "." ]]; then
            echo "putting $PWD/$app in $PWD/../monster-ui/apps/$app"
            $(rm $PWD/../monster-ui/apps/$app)
            $(ln -s $PWD/$app $PWD/../monster-ui/apps/$app)
        fi
    done
    popd > /dev/null
}

function setup_monster {
    if [ ! -d monster-ui ]; then
        echo "setting up monster-ui"
        $(git clone https://github.com/2600hz/monster-ui monster-ui)
        $(mkdir -p monster-ui/apps)
    fi

    update_repo "monster-ui"
}

function setup_repos {
    if [ ! -d $APPS_PATH ]; then
        $(mkdir $APPS_PATH)
    fi

    pushd $APPS_PATH > /dev/null
    for REPO in ${REPOS[@]}; do
        setup_repo $REPO
        echo ""
    done
    popd > /dev/null
}

setup_monster

setup_repos

sync_to_monster

popd > /dev/null
