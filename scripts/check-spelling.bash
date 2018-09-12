#!/bin/bash

pushd "$(dirname "$0")" >/dev/null

ROOT="$(pwd -P)"/..
cd $ROOT

# from https://en.wikipedia.org/wiki/Commonly_misspelled_English_words
FILE="$ROOT/scripts/misspellings.txt"
CHANGED=${CHANGED:-$(git --no-pager diff --name-only HEAD origin/4.3 -- $ROOT/applications $ROOT/core $ROOT/doc)}

function check_spelling {
    correct=$(echo "$1" | cut -f1 -d"|")
    bad=$(echo "$1" | cut -f2 -d"|")
    bad_grep=${bad// /|}
    bad_sed=${bad// /\\|}

    while IFS= read f; do
        [ $(basename $f) = $(basename $FILE) ] && continue
        file %f | grep -q "ASCII text" || continue
        echo "  fixing $f with $correct"
        sed -i "s/$bad_sed/$correct/g" $f
    done < <(echo $CHANGED | xargs egrep --no-messages -lw "$bad_grep" )
}

echo "checking spelling:"
while read LINE; do
    check_spelling "$LINE"
done < $FILE

popd >/dev/null
