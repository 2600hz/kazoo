/*
Section: Crossbar
Title: Services
Language: en-US
Version: 3.20
*/

# Services

Fetch the current services:

    curl -v -X GET -H "x-auth-token: {AUTH_TOKEN}" http://{SERVER}/v2/accounts/{ACCOUNT_ID}/services

Update the billing ID:

    curl -v -X POST -H "x-auth-token: {AUTH_TOKEN}" -H "content-type:application/json" http://{SERVER}/v2/accounts/{ACCOUNT_ID}/services -d '{"data":{"billing_id":"{BILLING_ID}"}}'

Fetch the audit logs for the account:

    curl -v -X GET -H "x-auth-token: {AUTH_TOKEN}" http://{SERVER}/v2/accounts/{ACCOUNT_ID}/services/audit
