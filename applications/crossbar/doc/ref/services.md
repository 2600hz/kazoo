# Services

## About Services

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/manual

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/manual
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/manual

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/manual
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/services/manual

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/manual
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/overrides

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/overrides
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/overrides

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/overrides
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/services/overrides

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/overrides
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/quote

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/quote
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/reconciliation

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/reconciliation
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/synchronization

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/synchronization
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/topup

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/topup
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/audit_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit_summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/audit

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/available

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/available
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/editable

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/editable
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/audit_summary/{SOURCE_SERVICE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit_summary/{SOURCE_SERVICE}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/audit/{AUDIT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit/{AUDIT_ID}
```

