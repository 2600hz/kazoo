#!/bin/bash

grep -Rl "whistle" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/whistle/kazoo/g'
grep -Rl "whapps" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/whapps/kapps/g'
grep -Rl "wapi" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/wapi/kapi/g'
grep -Rl "wh_" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/wh_/kz_/g'

grep -Rl "WHISTLE" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/WHISTLE/KAZOO/g'
grep -Rl "WHAPPS" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/WHAPPS/KAPPS/g'
grep -Rl "WAPI" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/WAPI/KAPI/g'
grep -Rl "WH_" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/WH_/KZ_/g'

grep -Rl "Whistle" * | grep -v "beam" | grep -v ".*\.log" | grep -v "wh_to_kz.sh" | xargs sed -i 's/Whistle/Kazoo/g'

find . -name "whistle*" | xargs rename 's/whistle/kazoo/g'
find . -name "whapps*" | xargs rename 's/whapps/kapps/g'
find . -name "wapi*" | xargs rename 's/wapi/kapi/g'
find . -name "wh_*" | grep -v "wh_to_kz.sh" | xargs rename 's/wh_/kz_/g'
