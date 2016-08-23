#!/bin/bash

[[ $# -eq 0 ]] && echo "Usage: $0  ‹path to check›+" && exit 0

function check_andalso_orelse {
    echo 'Check for andalso/orelse dropped lines'
    ! grep -Ern '[a-zA-Z\)] +(andalso|orelse)' -- $@
}

function check_MODULE {
    echo 'Check for uses of module in lieu of ?MODULE'
    local errors=0
    for f in "$@"; do
        m0=$(grep -E module'\(' "$f"  2>/dev/null)
        [[ $? -ne 0 ]] && continue
        local err=0
        m=$(echo $m0 | cut -d'(' -f2 | cut -d')' -f1)
        grep -nE '^[^%]*[^a-zA-Z0-9_]'$m: "$f"
        [[ $? -ne 1 ]] && ((err++))
        grep -nE "^[^%]*'$m'" "$f"
        [[ $? -ne 1 ]] && ((err++))
        [[ $err -ne 0 ]] && echo "^ $f" && echo && ((errors++))
    done
    return $errors
}

errs=0
check_andalso_orelse "$@" || ((errs++))
check_MODULE "$@" || ((errs++))
exit $errs
