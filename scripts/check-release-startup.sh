#!/bin/bash -e

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@$(hostname -f)
[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

echo "Checking release startup with node $rel..."

sup() {
    result=$(bash -c "$PWD"/core/sup/priv/sup "$*" 2>&1)
    echo $result
}

shutdown() {
    sup -t 1000 init stop
}

script() {
    sup -t 120000 crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!' || shutdown
    sleep 3
#    sup kazoo_perf_maintenance json_metrics | python -m json.tool
    sleep 1
#    sup kazoo_perf_maintenance graphite_metrics 'compte_maitre' 'clu1' 'royaume'
    sleep 1
    sup -t 600000 kapps_maintenance migrate || shutdown
    sleep 3
    sup -t 600000 kapps_maintenance migrate_to_4_0 || shutdown
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
