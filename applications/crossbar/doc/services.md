# Services

## About Services

Fetch and inspect account's service.

## Get Account Service

> GET /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

## Update the Account Billing ID

> POST /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "billing_id":"{BILLING_ID}"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

## Get Account Service Plan

> GET /v2/accounts/{ACCOUNT_ID}/services/plan

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/plan
```

## Get Account Service Status

> GET /v2/accounts/{ACCOUNT_ID}/services/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/status
```

#### Fetch Service Audit Logs

> GET /v2/accounts/{ACCOUNT_ID}/services/audit

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit
```
