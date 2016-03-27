#!/bin/bash -x

runtime0=10  # seconds
runtime1=11  # seconds
runtime2=12  # seconds
rel=${REL:-whistle_apps}  # whistle_apps | ecallmgr | ...

[[ ! -d _rel ]] && echo 'Cannot find _rel/ Is the release built?' && exit -1

echo 'Checking the startup of the release...'

sleep $runtime0 && ACT=stop REL=$rel make release &
sleep $runtime1 && ACT=stop REL=$rel make release &
sleep $runtime2 && ACT=stop REL=$rel make release &

REL=$rel make release
