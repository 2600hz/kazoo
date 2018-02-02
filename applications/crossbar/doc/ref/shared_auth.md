# Shared Authentication

## About Shared Authentication

#### Schema

Provides a local auth-token via a shared auth-token



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`shared_auth` | The shared token | `string(64)` |   | `true` |  



## Fetch

> GET /v2/shared_auth

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/shared_auth
```

## Create

> PUT /v2/shared_auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/shared_auth
```

