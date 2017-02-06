#!/bin/bash

pushd $(dirname $0) > /dev/null

ROOT=$(pwd -P)/..

function replace_call {
    FROM=$1
    TO=$2
    OLD_FUN=$3
    NEW_FUN=${3%$4}
    FILE=$5

    #echo "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g"
    $(sed -i "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g" $FILE)
}

function search_and_replace {
    declare -a FUNS=("${!1}")
    FROM=$2
    TO=$3
    SUFFIX=$4

    for FUN in "${FUNS[@]}"; do
        for FILE in `grep -rl "$FROM:$FUN" $ROOT/{core,applications}`; do
            replace_call $FROM $TO "$FUN" "$SUFFIX" $FILE
        done
    done
}

function replace_call_prefix {
    FROM=$1
    TO=$2
    OLD_FUN=$3
    NEW_FUN=${3#$4}
    FILE=$5

    #echo "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g"
    $(sed -i "s/$FROM:$OLD_FUN/$TO:$NEW_FUN/g" $FILE)
}

function search_and_replace_prefix {
    declare -a FUNS=("${!1}")
    FROM=$2
    TO=$3
    PREFIX=$4

    for FUN in "${FUNS[@]}"; do
        for FILE in `grep -rl "$FROM:$FUN" $ROOT/{core,applications}`; do
            replace_call_prefix $FROM $TO "$FUN" "$PREFIX" $FILE
        done
    done
}

# Functions moved from kz_util into more appropriately-named modules
# Run this to convert references from kz_util:* to the new module names

function kz_util_to_term {
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
    search_and_replace fs[@] "kz_util" "kz_term" ""
}

function kz_util_to_binary {
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
    search_and_replace fs[@] "kz_util" "kz_binary" "_binary"
    search_and_replace special[@] "kz_util" "kz_binary" "binary_"
    search_and_replace_prefix special[@] "kz_binary" "kz_binary" "binary_"
}

function kz_util_to_time {
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
    search_and_replace fs[@] "kz_util" "kz_time" ""
}

echo "ensuring kz_term is used"
kz_util_to_term
echo "ensuring kz_binary is used"
kz_util_to_binary
echo "ensuring kz_time is used"
kz_util_to_time

popd > /dev/null
