#!/bin/bash

# Prerequisites check for make

# Check git version
GIT_MAJOR_VERSION=2
if type git > /dev/null 2>&1; then
    MajorVersion=`git --version | awk '{print $3}' | awk -F'.' '{print $1}'`
    if [ $MajorVersion -ne $GIT_MAJOR_VERSION ]
    then
        echo "git major version ${GIT_MAJOR_VERSION} is required."
        exit 1
    fi
else
    echo "git is missing"
    exit 1
fi