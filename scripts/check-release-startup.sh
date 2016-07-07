#!/bin/bash

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

echo 'Checking the startup of the release...'

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@127.0.0.1

[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

function stuff() {
    erl -noshell -setcookie change_me -name doer@${rel##*@} -eval "ok = rpc:call('$rel', crossbar_maintenance, create_account, [<<\"compte_maitre\">>, <<\"royaume\">>, <<\"superduperuser\">>, <<\"pwd!\">>])." -s init stop
}

function stop() {
    erl -noshell -setcookie change_me -name stopper@${rel##*@} -eval "ok = rpc:call('$rel', init, stop, [])." -s init stop
}

sleep 180 && stuff &
sleep 220 && stop &

export KAZOO_CONFIG=rel/ci-config.ini
REL=$rel make release
