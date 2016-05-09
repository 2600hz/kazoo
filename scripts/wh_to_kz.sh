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

kz_term_exports='always_false always_true binary_md5 ceiling clean_binary floor from_hex_binary from_hex_string hexencode_binary identity is_boolean is_empty is_false is_not_empty is_proplist is_true join_binary lcfirst_binary pad_binary pad_binary_left rand_hex_binary remove_white_spaces shuffle_list strip_binary strip_left_binary strip_right_binary suffix_binary to_atom to_binary to_boolean to_float to_hex to_hex_binary to_integer to_list to_lower_binary to_lower_string to_number to_upper_binary to_upper_string truncate_binary truncate_left_binary truncate_right_binary ucfirst_binary'
util_split 'kz_term' $kz_term_exports


kz_time_exports='current_tstamp current_unix_tstamp decr_timeout elapsed_ms elapsed_s elapsed_us format_date format_datetime format_time gregorian_seconds_to_unix_seconds iso8601 microseconds_to_seconds milliseconds_to_seconds now now_ms now_s now_us pad_month pretty_print_datetime pretty_print_elapsed_s rfc1036 to_date to_datetime unix_seconds_to_gregorian_seconds unix_timestamp_to_gregorian_seconds'
util_split 'kz_time' $kz_time_exports


kz_accounts_exports='account_update disable_account enable_account format_account_db format_account_id format_account_mod_id format_account_modb get_account_realm is_account_enabled is_account_expired is_in_account_hierarchy is_in_account_hierarchy is_system_admin is_system_db maybe_disable_account normalize_account_name set_allow_number_additions set_superduper_admin'
util_split 'kz_accounts' $kz_accounts_exports


kz_http_util_from_kz_util='resolve_uri safe_urlencode uri uri_decode uri_encode'
util_split 'kz_http_util' $kz_http_util_from_kz_util
