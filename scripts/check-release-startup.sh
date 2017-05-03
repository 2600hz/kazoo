#!/bin/bash -e

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

echo 'Checking the startup of the release...'

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@127.0.0.1

[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

sup_() {
    local RET=$1; shift
    local M=$1; shift
    local F=$1; shift
    declare -a a=()
    for arg in "$@"; do a+=( '<<"'"$arg"'">>' ); done
    IFS=, eval 'A=${a[*]}'
    printf '\e[1;3m%s\e[0m\n' "# SUP $M $F $A"
    erl -noshell -setcookie change_me -name sup_$RANDOM@${rel##*@} -eval "$RET = rpc:call('$rel', $M, $F, [$A])." -s init stop
}

sleep 300 && sup_ 'ok' crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!' &
sleep 360 && sup_ 'no_return' kapps_maintenance migrate &
sleep 720 && sup_ 'ok' init stop &

export KAZOO_CONFIG=$PWD/rel/ci-config.ini
REL=$rel make release
code=$?

if [[ -f erl_crash.dump ]]; then
    echo A crash dump was generated!
    code=3
fi
error_log=$PWD/_rel/kazoo/log/error.log
if [[ -s $error_log ]]; then
    echo
    echo Error log:
    cat $error_log
    code=4
fi
exit $code
