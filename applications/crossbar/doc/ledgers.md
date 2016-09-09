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


#### List current Ledgers

List current ledgers and value for an account.

> GET /v2/accounts/{ACCOUNT_ID}/ledgers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers
```

```json
{
    "data": {
        "per-minute-voip": -825,
        "support": -148
    },
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Get Ledger values

List ledger values for an account with paging and filtering support

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/{LEDGER_ID}?created_from=11111111&created_to=22222222

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{LEDGER_ID}
```

```json
{
    "page_size": 30,
    "data": [
        {
            "source": {
                "service": "per-minute-voip",
                "id": "{CALL_ID}"
            },
            "account": {
                "id": "{ACCOUNT_ID}",
                "name": "{ACCOUNT_NAME}"
            },
            "usage": {
                "type": "voice",
                "quantity": 3,
                "unit": "sec"
            },
            "amount": 6,
            "description": "US Hollywood",
            "period": {
                "start": 63630348840
            },
            "id": "{DOC_ID}"
        }
    ],
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Get Ledger document

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/{LEDGER_ID}/{LEDGER_ENTRY_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{LEDGER_ID}/{LEDGER_ENTRY_ID}
```

```json
{
    "data": {
        "source": {
            "service": "per-minute-voip",
            "id": "{CALL_ID}"
        },
        "account": {
            "id": "{ACCOUNT_ID}",
            "name": "{ACCOUNT_NAME}"
        },
        "usage": {
            "type": "voice",
            "quantity": 3,
            "unit": "sec"
        },
        "amount": 6,
        "description": "US Hollywood",
        "period": {
            "start": 63630348840
        },
        "id": "{DOC_ID}"
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Credit / Debit

Credit or Debit a specific ledger.
the `account_id` for `AUTH_TOKEN` must be reseller of target account.

Parameter "impact_reseller" (boolean not required) when true will also create the document in the reseller

> PUT /v2/accounts/{ACCOUNT_ID}/ledgers/debit

> PUT /v2/accounts/{ACCOUNT_ID}/ledgers/credit

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/debit
```

```json
{
    "data": {
        "amount": 100,
        "description": "blablabla",
        "source": {
            "service": "tower/support/...",
            "id": "mac/mdn/..."
        },
        "usage": {
            "type": "data",
            "quantity": 5,
            "unit": "MB"
        },
        "period": {
            "start": 10938710938,
            "end": 214109238023899
        }
    },
    "impact_reseller": true
}
```
