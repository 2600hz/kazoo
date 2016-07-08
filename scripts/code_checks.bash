#!/bin/bash

[[ $# -eq 0 ]] && echo "Usage: $0  ‹path to check›+" && exit 0

function check_andalso_orelse {
    # Check for andalso/orelse dropped lines
    ! grep -Ern '[a-zA-Z\)] +(andalso|orelse)' -- $@
}

check_andalso_orelse "$@"
