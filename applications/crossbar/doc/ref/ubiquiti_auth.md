### Ubiquiti_auth

#### About Ubiquiti_auth

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`password` | Ubiquiti SSO Password | `string(1..64)` |   | `true`
`username` | Ubiquiti SSO Username | `string(1..64)` |   | `true`


#### Create

> PUT /v2/ubiquiti_auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/ubiquiti_auth
```

