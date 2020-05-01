#!/bin/bash

[[ $# -eq 0 ]] && echo "no changes for code checking" && echo "Usage: $0  ‹path to check›+" && exit 0
# find applications core -iname '*.erl' -or -iname '*.hrl' -or -iname '*.escript' -or -iname '*.app.src'

function P () {
    printf "\e[1;3m%s\e[0m\n" "$1"
}


function check_andalso_orelse {
    P 'Check for andalso/orelse dropped lines'
    ! grep -Ern '[^ %] +(andalso|orelse)' --include '*.escript' --include '*.erl' --include '*.hrl' --include '*.app.src' -- $@
}

function check_MODULE {
    P 'Check for uses of module in lieu of ?MODULE'
    local errors=0
    for f in "$@"; do
        base=$(basename "$f")
        [[ 'erl' != ${base##*.} ]] && continue
        local err=0
        m=$(grep -Fe '-module(' "$f"  2>/dev/null | cut -d'(' -f2 | cut -d')' -f1)
        grep -nHE '^[^%]*[^a-zA-Z0-9_]'$m[[:space:]]*: "$f"
        [[ $? -ne 1 ]] && ((err++))
        grep -nHE "^[^%]*'$m[[:space:]]*:'" "$f"
        [[ $? -ne 1 ]] && ((err++))
        [[ $err -ne 0 ]] && echo && ((errors++))
    done
    return $errors
}

function check_TABs {
    P 'Check for TAB characters'
    local errors=0
    for f in "$@"; do
        grep -Frn $'\t' --include '*.escript' --include '*.erl' --include '*.hrl' --include '*.app.src' -- "$f"
        [[ $? -ne 1 ]] && ((errors++))
    done
    return $errors
}

function check_trailing_whitespace {
    P 'Check for trailing whitespaces'
    ! grep -Ern '\s$' --include '*.escript' --include '*.erl' --include '*.hrl' --include '*.app.src' -- $@
}

declare -a args
args_i=0
for arg in "$@"; do
    [[ -e "$arg" ]] && args[$args_i]="$arg" && ((++args_i))
done
[[ $args_i -eq 0 ]] && exit 0

errs=0
check_andalso_orelse "${args[@]}" || ((errs++))
check_MODULE "${args[@]}" || ((errs++))
check_TABs "${args[@]}" || ((errs++))
check_trailing_whitespace "${args[@]}" || ((errs++))
exit $errs
