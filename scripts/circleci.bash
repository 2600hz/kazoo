#!/bin/bash

if [ ! -d $KAZOO_ROOT ]; then
    echo Cloning kazoo into $KAZOO_ROOT
    git clone https://github.com/2600hz/kazoo $KAZOO_ROOT
fi

if [ -f "$HOME/project/.base_branch" ]; then
    BASE_BRANCH="$(cat $HOME/project/.base_branch)"
else
    BASE_BRANCH="origin/master"
fi

cd $KAZOO_ROOT

echo resetting kazoo to $BASE_BRANCH
git fetch --prune
git rebase $BASE_BRANCH

if [ ! -d $APP_PATH ]; then
    echo adding submodule to $KAZOO_ROOT
    git submodule add ${CIRCLE_REPOSITORY_URL} $APP_PATH
fi

cd $APP_PATH

echo checking out our commit $CIRCLE_BRANCH
git fetch --prune
git checkout -B $CIRCLE_BRANCH
git reset --hard $CIRCLE_SHA1

cd $KAZOO_ROOT

# wanted when committing
echo setup git config
git config user.email 'circleci@dev.null'
git config user.name 'CircleCI'

echo committing kazoo changes to avoid false positives later
git add .gitmodules $APP_PATH
git commit -m "add submodule"

echo cleaning up kazoo
make -k clean
