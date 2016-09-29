#!/bin/bash

# Usage:  $0 ‹path to an app›

[[ $# -eq 0 ]] && $0 applications/*/ core/*/; exit $?

patterns=( ":start_link\\(\\{'local',\\s*" ":start_link\\(\\{local,\\s*" ':start_link\(' ':start_child\(' ':which_children\(' ':terminate_child\(' ':delete_child\(' ':call\(' ':cast\(' 'gen_listener:[^:\(]+\(' )
re='('
for pattern in ${patterns[@]}; do
    re="$re"$pattern'|'
done
re=${re%%|}')'

function registered() {
    local app="${1%%/}"
    local errors=0

    local regs=''; for f in $(\grep -IE -l -r ":start_link\\(\\{'local', \?SERVER" $app/src/); do regs="$regs, $(basename $f .erl)"; done; echo ${regs##, } | sort

    git grep -InE $re'\?MODULE' -- $app/src
    local count=0; [[ $? -eq 0 ]] && count=$(git grep -InE  $re'\?MODULE' -- $app/src | wc -l)
    ((errors += $count))

    local app_src=$app/src/*.app.src
    git grep -InE  '\{registered, \[\]\}' -- $app_src >/dev/null
    if [[ $? -eq 0 ]] && [[ -n "$regs" ]]; then
        echo $app has no registered modules??
        ((errors += 1))
    fi

    return $errors
}

all_errors=0
for arg in $*; do
    registered "$arg"
    errors=$?
    [[ $errors -ne 0 ]] && echo $errors errors
    ((all_errors += $errors))
done

echo $all_errors errors 'in' total
exit $all_errors
