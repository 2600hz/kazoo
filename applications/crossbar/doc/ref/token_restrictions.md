# Token Restrictions

## About Token Restrictions

#### Schema

Schema for token restrictions



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].allowed_accounts.[]` |   | `string()` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].allowed_accounts` | Account allowed to match this item | `array(string())` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].rules./^[\w/#*]+$/.[]` |   | `string('GET' | 'PUT' | 'POST' | 'PATCH' | 'DELETE' | '_')` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].rules./^[\w/#*]+$/` | verbs | `array(string('GET' | 'PUT' | 'POST' | 'PATCH' | 'DELETE' | '_'))` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].rules` | Rules applied to endpoint parameters | `object()` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/` |   | `array(object())` |   | `false` |  
`restrictions./^\w+$/./^\w+$/` | Level of user privilege. '_' matches any priv level | `object()` |   | `false` |  
`restrictions./^\w+$/` | Name of authentication method used when creating token. '_' matches any auth method | `object()` |   | `false` |  
`restrictions` |   | `object()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

