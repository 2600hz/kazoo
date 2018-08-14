# Schemas

## About Schemas

## Schema



## Fetch

> GET /v2/schemas

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas
```

## Fetch

> GET /v2/schemas/{SCHEMA_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/{SCHEMA_NAME}
```

## Create

> PUT /v2/schemas/{SCHEMA_NAME}/validation

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/{SCHEMA_NAME}/validation
```

