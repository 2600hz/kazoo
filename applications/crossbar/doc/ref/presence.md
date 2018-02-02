# Presence

## About Presence

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/presence

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/presence

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/presence/report-{REPORT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/report-{REPORT_ID}
```

