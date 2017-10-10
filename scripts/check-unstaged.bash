#!/bin/bash

if [ 0 -ne $(git status --porcelain | wc -l) ]; then
    echo Unstaged changes!
    git status --porcelain
    git --no-pager diff
    exit 1
fi
