#!/bin/bash

# Validate Swagger file using online validator

[[ $# -ne 1 ]] && echo "Usage: $0  <branch name>" && exit 1

URL='http://online.swagger.io/validator/debug?url=https://raw.githubusercontent.com/2600hz/kazoo/'$1'/applications/crossbar/priv/api/swagger.json'

tmp=$RANDOM.json
curl -o $tmp "$URL" || exit 2
"$(dirname "$0")"/format-json.sh $tmp || (rm $tmp && exit 3)
errors=$(cat $tmp | python2 -c 'import sys, json; print len(json.load(sys.stdin))')
[[ $errors -ne 0 ]] && echo Swagger file validation errors: $errors && cat $tmp
rm $tmp

exit $errors
