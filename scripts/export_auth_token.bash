#!/bin/bash

## Usage: eval $(./export_auth_token.bash -c [CREDENTIAL_HASH] -a [ACCOUNT_NAME])

## 1. First, export your credentials hash:
##    export CREDENTIALS=`echo -n "username:password" | md5sum | cut -d ' ' -f 1`
##    OR
##    Supply the hash as an argument to the script
## 2. eval the script as in the USAGE note above
## 3. Profit!

command -v jq >/dev/null 2>&1 || { echo >&2 "This script requires 'jq' be installed"; exit 1; }

usage() { echo 'Usage: eval $('"$0"' [-c {CREDENTIALS_HASH}] [-a {ACCOUNT_NAME}] [-p {PHONE_NUMBER}] [-r {ACCOUNT_REALM}])' 1>&2;}

function authenticate() {
    local C="$1"
    local TYPE="$2"
    local ID="$3"
    AUTH_RESP=$(curl -s -X PUT http://localhost:8000/v2/user_auth -d "{\"data\":{\"credentials\":\"$C\", \"$TYPE\":\"$ID\"}}")

    STATUS=$(echo $AUTH_RESP | jq -r '.status')

    if [[ "success" == $STATUS ]]; then
        echo "export ACCOUNT_ID=$(echo $AUTH_RESP | jq -r '.data.account_id')"
        echo "export AUTH_TOKEN=$(echo $AUTH_RESP | jq -r '.auth_token')"
    else
        echo $AUTH_RESP
    fi
}

while getopts ":a:c:p:r" opt; do
    case $opt in
        c)
            CREDS=${OPTARG}
            ;;
        p)
            IDENTIFIER_VALUE=${OPTARG}
            ACCOUNT_IDENTIFIER="phone_number"
            ;;
        a)
            IDENTIFIER_VALUE=${OPTARG}
            ACCOUNT_IDENTIFIER="account_name"
            ;;
        r)
            IDENTIFIER_VALUE=${OPTARG}
            ACCOUNT_IDENTIFIER="account_realm"
            ;;
        *)
            usage
            ;;
    esac
done

if [[ -z "${CREDS}" ]]; then
    CREDS="$CREDENTIALS"
fi

if [[ -z "${ACCOUNT_IDENTIFIER}" ]]; then
    usage
else
    authenticate $CREDS $ACCOUNT_IDENTIFIER $IDENTIFIER_VALUE
fi
