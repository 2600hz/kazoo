#!/bin/bash -e

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

rel=${REL:-kazoo_apps}  # kazoo_apps | ecallmgr | ...
[[ $rel != *@* ]] && rel=$rel@$(hostname -f)
[[ $rel != kazoo_apps* ]] && export KAZOO_APPS='ecallmgr'

echo "Checking release startup with node $rel..."

sup_() {
    printf '\e[1;3m%s\e[0m\n' "# SUP $*"
    RELX_REPLACE_OS_VARS=true KZname="-name $rel" exec $PWD/_rel/kazoo/bin/kazoo escript lib/sup-*/priv/sup.escript "$*"
}

sleep 300 && sup_ crossbar_maintenance create_account 'compte_maitre' 'royaume' 'superduperuser' 'pwd!' &
sleep 360 && sup_ kapps_maintenance migrate &
sleep 720 && sup_ init stop &

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
