#!/bin/bash

pushd $(dirname $0) > /dev/null

ROOT=(pwd -P)/..

EXCLUDES="merl.erl"

echo "Looking at $ROOT"
rm -f $ROOT/sar.txt

function SaRF {
    #SEARCH=$1
    #REPLACE=$2
    #FILE=$3
    #PREFIX=$4

    echo "'$4' '$1' '$2' in $3" >> $ROOT/sar.txt

    case ${FILE##*.} in
        erl )
            sed -i "s/$4$1/$4$2/g" $3 ;;
        hrl )
            sed -i "s/$4$1/$4$2/g" $3 ;;
    esac
}

function SaR {
    SEARCH=$1
    REPLACE=$2

    ## declare an array variable
    declare -a prefixes=(" " "(" "{" "=")

    ## now loop through the above array
    for PREFIX in "${prefixes[@]}"; do
        for FILE in $(grep -lr "$PREFIX$SEARCH" $ROOT/{core,applications,deps} --exclude=$EXCLUDES); do
            SaRF $SEARCH $REPLACE $FILE "$PREFIX"
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

echo "replacing erlang:now()"
SaR "erlang:now" "erlang:timestamp"

echo "replacing raw now()"
SaR "now()" "erlang:timestamp()"

# echo "replacing RabbitMQ ?QUEUE()"
# SaR "?QUEUE()" "queue:queue()"

# echo "replacing RabbitMQ ?DICT()"
# SaR "?DICT()" "dict:dict()"

# echo "replacing RabbitMQ ?SET()"
# SaR "?SET()" "sets:set()"

echo "replacing apns Queue::queue()"
SaRF "Queue::queue()" "Queue::queue:queue()" "$ROOT/applications/pusher/lib/apns/src/apns_queue.erl" ""

popd > /dev/null
