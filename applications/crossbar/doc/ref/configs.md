# Configs

## About Configs

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{CONFIG_ID}
```

