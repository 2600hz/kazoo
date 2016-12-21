### Api_auth

#### About Api_auth

#### Schema

Provides an auth-token via an Account API key

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`api_key` | The Accounts API key | `string(64)` |   | `true`




#### Create

> PUT /v2/api_auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/api_auth
```

