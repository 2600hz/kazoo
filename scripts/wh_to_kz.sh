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



function util_split() {
    local ToModule=$1; shift
    local FNames=$*
    for f in $FNames; do
        grep -Rl 'kz_util:' | xargs sed -i 's/kz_util:'$f'(/'$ToModule':'$f'(/g'
        grep -Rl 'kz_util:' | xargs sed -i 's%kz_util:'$f'/%'$ToModule':'$f'/%g'
    done
}

kz_term_exports='shuffle_list to_integer to_float to_number to_hex to_hex_binary rand_hex_binary hexencode_binary from_hex_binary from_hex_string to_list to_binary to_atom to_boolean is_boolean is_true is_false is_empty is_not_empty is_proplist identity always_true always_false to_lower_binary to_upper_binary to_lower_string to_upper_string ucfirst_binary lcfirst_binary strip_binary strip_left_binary strip_right_binary suffix_binary truncate_binary truncate_left_binary truncate_right_binary clean_binary remove_white_spaces binary_md5 pad_binary pad_binary_left join_binary floor ceiling'
util_split 'kz_term' $kz_term_exports
