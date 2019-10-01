#!/bin/bash

if [ ! -d $KAZOO_ROOT ]; then
    echo Cloning kazoo into $KAZOO_ROOT
    git clone https://github.com/2600hz/kazoo $KAZOO_ROOT
fi

## TODO: when stable core is ready use this
#if [[ $BASE_BRANCH != "origin/master" ]]; then
#    CORE_IDENTITY=`curl https://api.github.com/repos/2600hz/kazoo/git/refs/tags | grep "refs" | sed 's|[^0-9\.]||g' | sort --version-sort | grep "${BASE_BRANCH#origin/}" | tail -1`
#else
#    CORE_IDENTITY='master'
#fi
## instead of this
if [[ -z $BASE_BRANCH ]]; then
    BASE_BRANCH='origin/master'
fi

cd $KAZOO_ROOT

## TODO: when stable core is ready use this
#echo resetting kazoo-core to $CORE_IDENTITY
#git fetch --prune
#git checkout $CORE_IDENTITY

echo resetting kazoo-core to $BASE_BRANCH
git fetch --prune
git checkout $BASE_BRANCH

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

echo "make a pristine environment for the kazoo dir"
git clean -x -d -f

# wanted when committing
echo setup git config
git config user.email 'circleci@dev.null'
git config user.name 'CircleCI'

echo committing kazoo changes to avoid false positives later
git add .gitmodules $APP_PATH
git commit -m "add submodule"
