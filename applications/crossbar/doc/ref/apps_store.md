# Apps Store

## About Apps Store

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/apps_store

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/blacklist

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/blacklist
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/apps_store/blacklist

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/blacklist
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/icon

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/icon
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/screenshot/{APP_SCREENSHOT_INDEX}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/screenshot/{APP_SCREENSHOT_INDEX}
```

