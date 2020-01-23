#!/bin/bash

set -e

pushd "$(dirname "$0")" >/dev/null

ROOT=$(readlink -f "$(pwd -P)"/..)

errors=0
missing=""

REF_DIRS=$(find $ROOT/core $ROOT/applications -name "ref" -type d)

for REF_DIR in $REF_DIRS; do
    for REF_DIFF in $(git --no-pager diff --name-only HEAD -- $REF_DIR); do
        DOC=$(dirname $(dirname $REF_DIFF))/$(basename $REF_DIFF)
        DOC_DIFF=$(git --no-pager diff --name-only HEAD -- $DOC)
        if [ -f $DOC ] && [ -z $DOC_DIFF ]; then
            missing="$missing $DOC:1: "$'\n'
            errors=1
        fi
    done
done

if [ $errors = 1 ]; then
    echo "The following .md files are missing changes from their ref doc:"
    echo $missing
    popd >/dev/null
    exit $errors
fi

missing=""

declare -A ref_map
# maps ref doc name to doc name when not the same
ref_map=([vmboxes.md]=voicemail.md \
                     [conferences.md]=conference.md \
                     [whitelabel.md]=whitelabeling.md \
                     [user_auth.md]=user_authentication.md \
                     [api_auth.md]=api_authentication.md \
        )

for REF_DIR in $REF_DIRS; do
    for REF in $(find $REF_DIR -name "*.md"); do
        DOC=$(dirname $(dirname $REF))/$(basename $REF)
        if [ ${ref_map[$(basename $REF)]} ]; then DOC=$(dirname $(dirname $REF))/${ref_map[$(basename $REF)]}; fi

        if [ ! -f "$DOC" ]; then
            missing="$missing $DOC - creating it from $REF"$'\n'
            cp -a $REF $DOC
            errors=1
        fi
    done
done

if [ $errors = 1 ]; then
    echo "The following docs were auto-created from their ref versions:"
    echo "$missing"

    popd >/dev/null

    exit $errors
fi


popd >/dev/null
