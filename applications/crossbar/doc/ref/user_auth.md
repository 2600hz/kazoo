# User Authentication

## About User Authentication

#### Schema

Provides an auth-token via user credentials



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account_name` | The account name of the user | `string(1..128)` |   | `false` |  
`account_realm` | The account realm of the user | `string(4..253)` |   | `false` |  
`credentials` | A hash of the user credentials | `string(1..64)` |   | `true` |  
`method` | The hash method | `string('md5' | 'sha')` | `md5` | `false` |  
`phone_number` | A phone number assigned to the users account | `string(1..64)` |   | `false` |  



## Create

> PUT /v2/user_auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/user_auth
```

## Fetch

> GET /v2/user_auth/{AUTH_TOKEN}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/user_auth/{AUTH_TOKEN}
```

## Create

> PUT /v2/user_auth/recovery

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/user_auth/recovery
```

## Change

> POST /v2/user_auth/recovery

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/user_auth/recovery
```

