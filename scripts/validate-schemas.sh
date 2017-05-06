#!/bin/bash -e
# -*- coding: utf-8 -*-

# Validate JSON schemas using python2's jsonschema tool

validate() {
    local file="$1"
    if ! jsonschema "$file" 2>&1 >/dev/null; then
        echo
        echo Bad schema: "$file"
        cat "$file"
        echo
        echo Run again with:
        echo "$0 $file"
        exit 2
    fi
}

until [[ -z "$1" ]]; do
    if [[ -d "$1" ]]; then
        for schema in "$1"/*.json; do
            validate "$schema"
        done
    elif [[ -f "$1" ]]; then
        validate "$1"
    fi
    shift
done
