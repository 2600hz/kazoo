#!/bin/bash -e

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@$(hostname -f)
[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

echo "Checking release startup with node $rel..."

sup() {
    "$PWD"/core/sup/priv/sup "$*"
}

shutdown() {
    sup init stop
}

script() {
    sup crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!' || shutdown
    sleep 3
    sup kapps_maintenance migrate || shutdown
    sleep 3
    sup kapps_maintenance migrate_to_4_0 || shutdown
    sleep 9
    shutdown
}

sleep 240 && script &
export KAZOO_CONFIG=$PWD/rel/ci-config.ini
REL=$rel make release
code=$?

if [[ -f erl_crash.dump ]]; then
    echo A crash dump was generated!
    code=3
fi

error_log="$PWD/_rel/kazoo/log/error.log"
if [[ -f $error_log ]]; then
    echo
    echo Error log:
    cat "$error_log"
    if [[ $(grep -c -v -F 'exit with reason shutdown' "$error_log") -gt 0 ]]; then
        echo
        echo "Found errors in $error_log"
        code=4
    fi
fi

exit $code
