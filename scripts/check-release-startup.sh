#!/bin/bash -e
[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

echo "Checking release startup..."

REL_DIR=$(dirname $(readlink -f $0))/../_rel/kazoo
PATH=$PATH:$(dirname $0):${REL_DIR}/bin
export PATH
LOG_DIR=${REL_DIR}/log
export LOG_DIR
mkdir -p ${LOG_DIR}

shutdown() {
    sup init stop
    exit 0
}

waitfor() {
TIMEOUT=${1:-"120"}
SEARCH_TERM=${2:-"nada"}
echo "waiting for '$SEARCH_TERM'"
(timeout --foreground $TIMEOUT tail -f ${LOG_DIR}/debug.log  2>&1 &) | grep -q "$SEARCH_TERM" && echo "found '$SEARCH_TERM'" && return 0
echo "timeout waiting for '$SEARCH_TERM'" > ${LOG_DIR}/error.log
echo "timeout waiting for '$SEARCH_TERM'"
return 1
}


script() {
    touch ${LOG_DIR}/debug.log
    touch ${LOG_DIR}/error.log
    waitfor 180 "auto-started kapps" || shutdown
    echo "creating account"
    sup crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!' || shutdown
    echo "running account created"
    sleep 3
    echo "running migrate"
    sup kapps_maintenance migrate || shutdown
    sleep 3
    echo "running migrate to 4"
    sup kapps_maintenance migrate_to_4_0 || shutdown
    sleep 9
    shutdown
}

export KAZOO_NODE=apps
export KAZOO_COOKIE=change_me

script > _rel/kazoo/log/sup.log 2>${LOG_DIR}/error.log &

export KAZOO_CONFIG=$(dirname $(readlink -f $0))/../rel/ci.config.ini
kazoo console

code=$?

if [[ -f erl_crash.dump ]]; then
    echo A crash dump was generated!
    code=3
fi

error_log="${LOG_DIR}/error.log"
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
