### Token_restrictions

#### About Token_restrictions

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`restrictions` |   | `object` |   | `false`


#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/token_restrictions

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/token_restrictions

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/token_restrictions

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

