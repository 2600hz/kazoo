#!/bin/bash

# Validate Swagger file using online validator

URL='http://online.swagger.io/validator/debug'
SWAGGER=applications/crossbar/priv/api/swagger.json

tmp=$RANDOM.json
curl -o $tmp "$URL" -X POST -d @$SWAGGER -H 'Content-Type:application/json' || exit 2
"$(dirname "$0")"/format-json.py $tmp || (rm $tmp && exit 3)
errors=$(cat $tmp | python3 -c 'import sys, json; print(len(json.load(sys.stdin).get("schemaValidationMessages", [])))')
[[ $errors -ne 0 ]] && echo Swagger file validation errors: $errors && cat $tmp
rm $tmp

exit $errors
