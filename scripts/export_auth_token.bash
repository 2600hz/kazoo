#!/bin/bash

## Usage: $(eval ./export_auth_token.bash -c [CREDENTIAL_HASH] -a [ACCOUNT_NAME])

## 1. First, export your credentials hash:
##    export CREDENTIALS=`echo -n "username:password" | md5sum | cut -d ' ' -f 1`
##    OR
##    Supply the hash as an argument to the script
## 2. eval the script as in the USAGE note above
## 3. Profit!

set -e

: ${SERVER:=http://localhost:8000}
: ${VERBOSE:=false}
: ${CONNECT_TIMEOUT:=30}

command -v jq >/dev/null 2>&1 || { echo >&2 "This script requires 'jq' be installed"; exit 1; }

usage() {
    cat <<END >&2
Gets an auth token and outputs Bash script to export the following environment
variables:

    ACCOUNT_ID
    AUTH_TOKEN

Usage:

\$(eval $0 [options])

Options:

-v:                      Print extra information (verbose)
-c CREDENTIALS_HASH      \$(echo -n "username:password" | md5sum | cut -d ' ' -f 1)
-a ACCOUNT_NAME
-p PHONE_NUMBER
-r ACCOUNT_REALM
-k API_KEY
-s BASE_URL             Defaults to ${SERVER}; include the http(s) scheme
END
    exit 1
}

verbose() { $VERBOSE; }
connect_timeout() { echo $CONNECT_TIMEOUT; }

# Declare a named variable and set its content to
# the input stream. Output the attributes and value of
# the declared variable in the format
#
#   declare -- varname="Contents of input stream"
#
# Eval the output to actually declare the variable.
#
# Example
# -------
# $ xyzzy() { eval $(echo "This is a test" | setval x); echo "x=$x"; }
# $ xyzzy
# x=This is a test
#
# Note that because the eval is done within a function, the variable
# 'x' in the example above is local to function xyzzy.
#
# $1: Variable name to declare
setval() {
    printf -v "$1" "%s" "$(cat)"; declare -p "$1";
}

# PUT to $url with data $data.
#
# Send status line '%{response_code}|%{content_type}'
# to stdout, and received data to stderr.
#
# Status line will look like '200|application/json; ...'
#
# Use with setval() as follows:
# eval "$(put_url "$url" "${data}" 2> >(setval json) 1> >(setval status_line))"
#
# Remarks
# -------
# setval() is trivial to implement, yet a bit subtle:
#
# setval() {
#    printf -v "$1" "%s" "$(cat)"; declare -p "$1";
# }
put_url() {
    local url="$1"; shift
    local data="$1"; shift
    local timeout="$1";
    local format_str='%{response_code}|%{content_type}'

    curl \
        -s \
        -L \
        --connect-timeout ${timeout:-30} \
        -X PUT \
        --write-out "$format_str" \
        --output /dev/stderr \
        -d "${data}" \
        "$url"
}

http_status_ok() {
    local -i status="$1"
    (( status >= 200 && status < 300 ))
}

http_status_text() {
    local -i http_code="$1"; shift
    local timeout=$(connect_timeout)
    local status

    case ${http_code} in
         000) status="Not responding within ${timeout} seconds" ;;
         100) status="Informational: Continue" ;;
         101) status="Informational: Switching Protocols" ;;
         200) status="Successful: OK within ${timeout} seconds" ;;
         201) status="Successful: Created" ;;
         202) status="Successful: Accepted" ;;
         203) status="Successful: Non-Authoritative Information" ;;
         204) status="Successful: No Content" ;;
         205) status="Successful: Reset Content" ;;
         206) status="Successful: Partial Content" ;;
         300) status="Redirection: Multiple Choices" ;;
         301) status="Redirection: Moved Permanently" ;;
         302) status="Redirection: Found residing temporarily under different URI" ;;
         303) status="Redirection: See Other" ;;
         304) status="Redirection: Not Modified" ;;
         305) status="Redirection: Use Proxy" ;;
         306) status="Redirection: status not defined" ;;
         307) status="Redirection: Temporary Redirect" ;;
         400) status="Client Error: Bad Request" ;;
         401) status="Client Error: Unauthorized" ;;
         402) status="Client Error: Payment Required" ;;
         403) status="Client Error: Forbidden" ;;
         404) status="Client Error: Not Found" ;;
         405) status="Client Error: Method Not Allowed" ;;
         406) status="Client Error: Not Acceptable" ;;
         407) status="Client Error: Proxy Authentication Required" ;;
         408) status="Client Error: Request Timeout within ${timeout} seconds" ;;
         409) status="Client Error: Conflict" ;;
         410) status="Client Error: Gone" ;;
         411) status="Client Error: Length Required" ;;
         412) status="Client Error: Precondition Failed" ;;
         413) status="Client Error: Request Entity Too Large" ;;
         414) status="Client Error: Request-URI Too Long" ;;
         415) status="Client Error: Unsupported Media Type" ;;
         416) status="Client Error: Requested Range Not Satisfiable" ;;
         417) status="Client Error: Expectation Failed" ;;
         500) status="Server Error: Internal Server Error" ;;
         501) status="Server Error: Not Implemented" ;;
         502) status="Server Error: Bad Gateway" ;;
         503) status="Server Error: Service Unavailable" ;;
         504) status="Server Error: Gateway Timeout within ${timeout} seconds" ;;
         505) status="Server Error: HTTP Version Not Supported" ;;
         *)   status="Unknown HTTP status code: ${http_code}"
    esac
    echo "${status}"
}

do_auth() {
    local auth_url="$1"; shift
    local data="$1"; shift

    verbose && echo "PUTting to URL '${auth_url}' with data '$data'" >&2 || true

    eval "$(put_url "$auth_url" "${data}" 2> >(setval json) 1> >(setval status_line))"

    verbose && echo "Received JSON: $(jq . <<<$json)" >&2 || true
    verbose && echo "Status line: ${status_line}" >&2 || true

    local status=${status_line%%|*}
    local content_type=${status_line##*|}
    local expected_content_type='application/json'

    if http_status_ok "${status}"; then
        if [[ "${content_type%%;*}" == "${expected_content_type}" ]]; then
            echo -n $json
        else
            printf 'Wrong content type; got "%s", expected "%s"\n' \
                "${content_type%%;*}" "${expected_content_type}" >&2
            exit 1
        fi
    elif [[ -n "${json}" ]]; then
        echo -n "${json}"
    else
        http_status_text ${status} >&2
        exit 1
    fi
}

export_auth_creds() {
    local auth_resp="$1"; shift

    echo "export ACCOUNT_ID=$(echo "${auth_resp}" | jq -r '.data.account_id')"
    echo "export AUTH_TOKEN=$(echo "${auth_resp}" | jq -r '.auth_token')"
}

api_authenticate() {
    local c="$1"; shift
    local base_url="$1"; shift

    local content_type_hdr='Content-Type: application/json'
    local data="{\"data\":{\"api_key\":\"${c}\"}}"
    local auth_url="${base_url}/v2/api_auth"
    local auth_resp
    local status

    auth_resp="$(do_auth "${auth_url}" "${data}")"
    status=$(echo $auth_resp | jq -r '.status')

    if [[ "success" == "${status}" ]]; then
        export_auth_creds "${auth_resp}"
    else
        echo "echo ${status:-failed}: ${auth_resp:-no auth response} && true"
    fi
}

user_authenticate() {
    local c="$1"; shift
    local type="$1"; shift
    local id="$1"; shift
    local base_url="$1"; shift

    local auth_url="${base_url}/v2/user_auth"
    local data="{\"data\":{\"credentials\":\"${c}\",\"${type}\":\"${id}\"}}"
    local auth_resp
    local status

    auth_resp="$(do_auth "${auth_url}" "${data}")"
    status=$(echo ${auth_resp} | jq -r '.status')

    if [[ "success" == "${status}" ]]; then
        export_auth_creds "${auth_resp}"
    else
        echo "echo ${status:-failed}: ${auth_resp:-no auth response} && true"
    fi
}

main() {
    local server="$1"; shift
    local creds="$1"; shift

    local identifier_value
    local account_identifier

    while getopts ":s:a:c:p:r:kv" opt; do
        case "$opt" in
            c)
                creds="${OPTARG}"
                ;;
            s)
                server="${OPTARG}"
                ;;
            p)
                identifier_value="${OPTARG}"
                account_identifier="phone_number"
                ;;
            a)
                identifier_value="${OPTARG}"
                account_identifier="account_name"
                ;;
            r)
                identifier_value="${OPTARG}"
                account_identifier="account_realm"
                ;;
            k)
                identifier_value="${OPTARG}"
                account_identifier="api_key"
                ;;
            v)
                VERBOSE="true"
                ;;
            *)
                usage
                ;;
        esac
    done

    if [[ -z "${account_identifier}" ]]; then
        usage
    elif [[ "api_key" == "${account_identifier}" ]]; then
        api_authenticate "${creds}" "${server}"
    else
        user_authenticate \
            "${creds}" "${account_identifier}" \
            "${identifier_value}" "${server}"
    fi
}

main "${SERVER}" "${CREDENTIALS:-}" "$@"
