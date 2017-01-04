### Shared_auth

#### About Shared_auth

#### Schema

Provides a local auth-token via a shared auth-token

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`shared_auth` | The shared token | `string(64)` |   | `true`




#### Detail a shared token

> GET /v2/shared_auth

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/shared_auth
```
