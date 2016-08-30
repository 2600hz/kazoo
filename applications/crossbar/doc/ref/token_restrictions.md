### Token_restrictions

#### About Token_restrictions

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`restrictions` |   | `object` |   | `false`
`restrictions.^\w+$` | Name of athentication metod used when creating token. "_" for match any auth method | `object` |   | `true`
`restrictions.^\w+$.^\w+$` | User privelege level. "_" for match any priv level | `object` |   | `true`
`restrictions.^\w+$.^\w+$.^\w+$` |   | `array(object)` |   | `true`
`restrictions.^\w+$.^\w+$.^\w+$.[].allowed_accounts` | Account allowed to match this item | `array(string)` |   | `false`
`restrictions.^\w+$.^\w+$.^\w+$.[].allowed_accounts.[]` |   | `string` |   | `false`
`restrictions.^\w+$.^\w+$.^\w+$.[].rules` | Rules applied to endpoint parameters | `object` |   | `false`
`restrictions.^\w+$.^\w+$.^\w+$.[].rules.^[\w/#*]+$` | verbs | `array(string('GET', 'PUT', 'POST', 'PATCH', 'DELETE', '_'))` |   | `false`
`restrictions.^\w+$.^\w+$.^\w+$.[].rules.^[\w/#*]+$.[]` |   | `string` |   | `false`


#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

