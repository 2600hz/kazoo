#!/bin/bash -e

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@$(hostname -f)
[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

echo "Checking release startup with node $rel..."

sup_() {
    RELX_REPLACE_OS_VARS=true KZname="-name $rel" $PWD/_rel/kazoo/bin/kazoo escript lib/sup-*/priv/sup.escript "$*"
}

script() {
    sup_ crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!'
    sleep 3
    sup_ kapps_maintenance migrate
    sleep 3
    sup_ kapps_maintenance migrate_to_4_0
    sleep 3
    sup_ init stop
}

sleep 240 && script &
export KAZOO_CONFIG=$PWD/rel/ci-config.ini
REL=$rel make release
code=$?

if [[ -f erl_crash.dump ]]; then
    echo A crash dump was generated!
    code=3
fi
error_log=$PWD/_rel/kazoo/log/error.log
echo
echo Error log:
cat $error_log
if [[ $(grep -c -v -F 'exit with reason shutdown' $error_log) -gt 0 ]]; then
    echo
    echo "Found errors in $error_log"
    code=4
fi
exit $code
