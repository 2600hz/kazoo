### Promised_payment

#### About Promised_payment

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`amount` |   | `number` | `0.0` | `true`
`armed` |   | `boolean` | `false` | `true`
`duration` |   | `integer` | `0` | `true`
`enabled` |   | `boolean` | `false` | `true`
`max_amount` |   | `number` | `0.0` | `true`
`max_duration` |   | `integer` | `86400` | `true`
`start` |   | `integer` | `0` | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/promised_payment

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/promised_payment
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/promised_payment

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/promised_payment
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/promised_payment

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/promised_payment
```

