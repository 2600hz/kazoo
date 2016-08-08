### Sms

#### About Sms

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`body` | text message | `string(1..700)` |   | `true`
`from` | caller-id-number, taken from user if absent | `string` |   | `false`
`scheduled` | The timestamp to start delivering the message | `integer` |   | `false`
`to` | callee-id-number | `string` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/sms

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/sms

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/sms/{_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms/{_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/sms/{_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms/{_ID}
```

