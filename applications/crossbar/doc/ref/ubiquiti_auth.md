# Ubiquiti Authentication

## About Ubiquiti Authentication

#### Schema

Provides an auth-token via Ubiquiti's SSO



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`password` | Ubiquiti SSO Password | `string(1..64)` |   | `true` |  
`username` | Ubiquiti SSO Username | `string(1..64)` |   | `true` |  



## Create

> PUT /v2/ubiquiti_auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/ubiquiti_auth
```

