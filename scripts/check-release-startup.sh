#!/bin/bash

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

echo 'Checking the startup of the release...'

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@127.0.0.1

[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

function sup_() {
    local M=$1; shift
    local F=$1; shift
    declare -a a=()
    for arg in "$@"; do a+=( '<<"'"$arg"'">>' ); done
    IFS=, eval 'A=${a[*]}'
    erl -noshell -setcookie change_me -name sup_$RANDOM@${rel##*@} -eval "ok = rpc:call('$rel', $M, $F, [$A])." -s init stop
}

sleep 360 && sup_ crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!' &
sleep 440 && sup_ init stop &

export KAZOO_CONFIG=rel/ci-config.ini
REL=$rel make release
