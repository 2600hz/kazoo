#!/bin/bash -e

pushd "$(dirname "$0")" >/dev/null

ROOT="$(pwd -P)"/..

replace() {
    local M0=$1
    local F0=$2
    local M1=$3
    local F1=$4
    for FILE in $(grep -Irl $M0:$F0 "$ROOT"/{core,applications}); do
        sed -i "s%$M0:$F0%$M1:$F1%g" "$FILE"
    done
}

replace_call() {
    FROM="$1"
    TO="$2"
    OLD_FUN="$3"
    NEW_FUN="${3%$4}"
    FILE="$5"

    #echo "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g"
    sed -i "s%$FROM:$OLD_FUN%$TO:$NEW_FUN%g" "$FILE"
}

search_and_replace() {
    declare -a FUNS=("${!1}")
    FROM="$2"
    TO="$3"
    SUFFIX="$4"

    for FUN in "${FUNS[@]}"; do
        for FILE in $(grep -Irl $FROM:$FUN "$ROOT"/{core,applications}); do
            replace_call $FROM $TO "$FUN" "$SUFFIX" "$FILE"
        done
    done
}

search_and_replace_exact() {
    declare -a FUNS=("${!1}")
    FROM=$2
    TO=$3
    TOFUN=$4

    for FUN in "${FUNS[@]}"; do
        for FILE in `grep -rl "$FROM:$FUN" $ROOT/{core,applications}`; do
            replace $FROM $TO "$FUN" "$TOFUN" $FILE
        done
    done
}

replace_call_prefix() {
    FROM="$1"
    TO="$2"
    OLD_FUN="$3"
    NEW_FUN="${3#$4}"
    FILE="$5"

    #echo "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g"
    sed -i "s%$FROM:$OLD_FUN%$TO:$NEW_FUN%g" "$FILE"
}

search_and_replace_prefix() {
    declare -a FUNS=("${!1}")
    FROM="$2"
    TO="$3"
    PREFIX="$4"

    for FUN in "${FUNS[@]}"; do
        for FILE in $(grep -Irl $FROM:$FUN "$ROOT"/{core,applications}); do
            replace_call_prefix $FROM $TO "$FUN" "$PREFIX" "$FILE"
        done
    done
}

replace_call_with_prefix() {
    FROM="$1"
    TO="$2"
    OLD_FUN="$3"
    NEW_FUN="$4$3"
    FILE="$5"

    #echo "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g"
    sed -i "s%$FROM:$OLD_FUN%$TO:$NEW_FUN%g" "$FILE"
}

search_and_replace_with_prefix() {
    declare -a FUNS=("${!1}")
    FROM="$2"
    TO="$3"
    PREFIX="$4"

    for FUN in "${FUNS[@]}"; do
        for FILE in $(grep -Irl $FROM:$FUN "$ROOT"/{core,applications}); do
            replace_call_with_prefix $FROM $TO "$FUN" "$PREFIX" "$FILE"
        done
    done
}

# Functions moved from kz_util into more appropriately-named modules
# Run this to convert references from kz_util:* to the new module names

kz_util_to_term() {
    local fs=(shuffle_list
              to_integer
              to_float
              to_number
              to_hex
              to_hex_binary
              to_hex_char
              to_list
              to_binary
              to_atom
              to_boolean
              to_date
              to_datetime
              to_lower_binary
              to_lower_string
              to_upper_binary
              to_upper_string
              to_upper_char
              to_lower_char
              error_to_binary

              is_true
              is_false
              is_boolean
              is_ne_binary
              is_empty
              is_not_empty
              is_proplist
              identity
              always_true
              always_false
              a1hash
              floor
              ceiling
             )
    search_and_replace fs[@] kz_util kz_term ''
}

kz_util_to_binary() {
    local fs=(rand_hex_binary
              hexencode_binary
              from_hex_binary
              ucfirst_binary
              lcfirst_binary
              strip_binary
              strip_left_binary
              strip_right_binary
              suffix_binary
              truncate_binary
              truncate_left_binary
              truncate_right_binary
              from_hex_string
              clean_binary
              remove_white_spaces
              binary_md5
              pad_binary
              pad_binary_left
              join_binary
              binary_reverse
             )
    local special=(binary_md5 binary_reverse)
    search_and_replace             fs[@] kz_util   kz_binary _binary
    search_and_replace        special[@] kz_util   kz_binary binary_
    search_and_replace_prefix special[@] kz_binary kz_binary binary_
}

kz_util_to_time() {
    local fs=(current_tstamp
              current_unix_tstamp
              decr_timeout
              elapsed_ms
              elapsed_ms
              elapsed_s
              elapsed_s
              elapsed_us
              elapsed_us
              format_date
              format_datetime
              format_time
              gregorian_seconds_to_unix_seconds
              iso8601
              microseconds_to_seconds
              milliseconds_to_seconds
              month
              now
              now_ms
              now_s
              now_us
              pad_month
              pretty_print_datetime
              pretty_print_elapsed_s
              rfc1036
              unitfy_seconds
              unix_seconds_to_gregorian_seconds
              unix_timestamp_to_gregorian_seconds
              weekday
             )
    search_and_replace fs[@] kz_util kz_time ''
}

kz_time_to_date() {
    local fs=(iso8601_date)
    local fs2=(pad_date
               pad_month
             )
    search_and_replace_exact fs[@] "kz_time" "kz_date" "to_iso8601_extended"
    search_and_replace fs2[@] "kz_time" "kz_date" ""
}

kz_json_to_kz_doc() {
    local fs=(get_public_keys
              public_fields
              private_fields
              is_private_key
             )
    search_and_replace fs[@] kz_json kz_doc ''
}

kz_json_to_kz_http() {
    local fs=(to_querystring)
    search_and_replace_with_prefix fs[@] kz_json kz_http_util json_
}

props_to_kz_http() {
    local fs=(to_querystring)
    search_and_replace_with_prefix fs[@] props kz_http_util props_
}

kapps_speech_to_kazoo_speech() {
    local fs=(create)

    local asrs=(asr_freeform
                asr_commands
               )

    search_and_replace fs[@] kapps_speech kazoo_tts ''
    search_and_replace_prefix asrs[@] kapps_speech kazoo_asr asr_
}

kz_media_recording_to_kzc_recording() {
    FROM=kz_media_recording
    TO=kzc_recording
    for FILE in $(grep -Irl $FROM: "$ROOT"/{core,applications}); do
            replace_call $FROM $TO '' '' "$FILE"
    done
}

kz_includes() {
    INCLUDES=(kz_databases.hrl
              kz_log.hrl
              kz_types.hrl
             )
    FROM=kazoo/include
    TO=kazoo_stdlib/include

    for FILE in $(grep -Irl $FROM/ "$ROOT"/{core,applications}); do
        for INCLUDE in "${INCLUDES[@]}"; do
            sed -i "s%$FROM/$INCLUDE%$TO/$INCLUDE%g" "$FILE"
        done
    done
}

dedupe() {
    replace crossbar_util get_account_doc kz_account fetch

    replace       kz_util get_account_realm kz_account fetch_realm
    replace crossbar_util get_account_realm kz_account fetch_realm

    replace  kapps_util get_account_name kz_account fetch_name
    replace kz_services account_name     kz_account fetch_name

    replace kapps_util get_event_type kz_util get_event_type
}

echo "ensuring kz_term is used"
kz_util_to_term
echo "ensuring kz_binary is used"
kz_util_to_binary
echo "ensuring kz_time is used"
kz_util_to_time
echo "ensuring kz_time -> kz_date migration is performed"
kz_time_to_date
echo "ensuring kz_json:public/private are moved to kz_doc"
kz_json_to_kz_doc
echo "ensuring kz_json:to_querystring is moved to kz_http_util"
kz_json_to_kz_http
echo "ensuring props:to_querystring is moved to kz_http_util"
props_to_kz_http
echo "ensuring kapps_speech to kazoo_speech"
kapps_speech_to_kazoo_speech
echo "ensuring kz_media_recording to kzc_recording"
kz_media_recording_to_kzc_recording
echo "ensuring includes from kazoo are moved to kazoo_stdlib"
kz_includes
echo 'ensuring utility calls are not duplicated all over the place'
dedupe

popd >/dev/null
