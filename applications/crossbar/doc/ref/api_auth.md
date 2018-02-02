# API Authentication

## About API Authentication

#### Schema

Provides an auth-token via an Account API key



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`api_key` | The Accounts API key | `string(64)` |   | `true` |  



## Create

> PUT /v2/api_auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/api_auth
```

