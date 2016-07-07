#!/bin/bash

[[ $# -eq 0 ]] && echo "Usage: $0  ‹path to check›+" && exit 1

function check_andalso_orelse {
    # Check for andalso/orelse dropped lines
    grep -Ern '[a-zA-Z\)] +(andalso|orelse)' "$@"
    [[ $? -eq 1 ]] && return 0
    return 1
}

check_andalso_orelse "$@"
