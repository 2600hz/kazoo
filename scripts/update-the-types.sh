#!/bin/bash

pushd `dirname $0` > /dev/null

ROOT=`pwd -P`/..

echo "Looking at $ROOT"

function SaR {
    SEARCH=$1
    REPLACE=$2

    for PREFIX in " " "("; do
        for FILE in `grep -lr "$PREFIX$SEARCH" $ROOT/{core,applications,deps}`; do
            case ${FILE##*.} in
                erl )
                    sed -i "s/$PREFIX$SEARCH/$PREFIX$REPLACE/g" $FILE ;;
                hrl )
                    sed -i "s/$PREFIX$SEARCH/$PREFIX$REPLACE/g" $FILE ;;
            esac
        done
    done
}

echo "replacing array()"
SaR "array()" "array:array()"

echo "replacing dict()"
SaR "dict()" "dict:dict()"

echo "replacing digraph()"
SaR "digraph()" "digraph:digraph()"

echo "replacing gb_set()"
SaR "gb_set()" "gb_sets:set()"

echo "replacing gb_tree()"
SaR "gb_tree()" "gb_trees:tree()"

echo "replacing queue()"
SaR "queue()" "queue:queue()"

echo "replacing tree()"
SaR "tree()" "trees:tree()"

echo "replacing set()"
SaR "set()" "sets:set()"

popd > /dev/null
