#!/bin/bash

# Install for alias 'tag-cl' with:
#   git config alias.tag-cl '!.scripts/tag_with_changelog.sh'

set -e # Abort on first failure, so we don't mess something up

if [ -z "$1" ]; then
    # Missing tag name
    echo "usage: git tag-cl <tag>" >&2
    exit 129
fi
if [ ! -f CHANGELOG ]; then
    # No changelog to be used
    echo "fatal: CHANGELOG missing" >&2
    exit 128
fi
if [ ! -z "$(git status --short)" ]; then
    # Sanity check
    echo "fatal: dirty repository" >&2
    exit 128
fi

CHANGELOG=$(cat CHANGELOG)

# Clean up changelog
echo "" > CHANGELOG
git add CHANGELOG

# Update version in .app file
sed -i "" -e "s/{vsn, .*}/{vsn, \"$1\"}/g" src/meck.app.src
sed -i "" -e "s/@version .*/@version $1/g" doc/overview.edoc
git add src/meck.app.src
git add doc/overview.edoc

# Commit, tag and push
git commit -m "Version $1"
git tag -s $1 -m "Version $

$CHANGELOG"
git push && git push --tags
