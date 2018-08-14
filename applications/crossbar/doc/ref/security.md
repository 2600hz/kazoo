# Security

## About Security

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/security/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security/attempts
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/security/attempts/{ATTEMPT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security/attempts/{ATTEMPT_ID}
```

