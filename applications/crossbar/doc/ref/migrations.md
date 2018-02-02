# Migrations

## About Migrations

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/migrations

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/migrations
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/migrations/{MIGRATION_ID}
```

