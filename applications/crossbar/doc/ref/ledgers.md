### Ledgers

#### About Ledgers

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account` | Account info | `object` |   | `false`
`account.id` | Account ID | `string` |   | `false`
`account.name` | Account name | `string` |   | `false`
`amount` | Ledger amount | `integer` |   | `false`
`description` | Useful description for ledger | `string` |   | `false`
`metadata` | Metadata for ledger document | `object` |   | `false`
`period` | Period of ledger | `object` |   | `false`
`period.end` | Period end | `integer` |   | `false`
`period.start` | Period start | `integer` |   | `false`
`source` | Origin of ledger | `object` |   | `true`
`source.id` | Source ID | `string` |   | `true`
`source.service` | Source service | `string` |   | `true`
`usage` | Usage for ledger | `object` |   | `true`
`usage.quantity` | Usage quantity | `integer` |   | `true`
`usage.type` | Usage type | `string` |   | `true`
`usage.unit` | Usage unit | `string` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ledgers

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/ledgers/debit

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/debit
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/ledgers/credit

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/credit
```

