# Tray

## About Tray

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tray

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/tray

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}
```

