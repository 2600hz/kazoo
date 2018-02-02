# Ledgers

## About Ledgers

The Ledgers API provides an easy way to see usage like per-minutes or flat-rate and manage your account's credit/debit values.

## Ledgers Schema

ledgers document

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account.id` | Account ID | `string()` |   | `false`
`account.name` | Account name | `string()` |   | `false`
`account` | Account info | `object()` |   | `false`
`amount` | Ledger amount | `integer()` |   | `false`
`description` | Useful description for ledger | `string()` |   | `false`
`metadata` | Metadata for ledger document | `object()` |   | `false`
`period.end` | Period end | `integer()` |   | `false`
`period.start` | Period start | `integer()` |   | `false`
`period` | Period of ledger | `object()` |   | `false`
`source.id` | Source ID | `string()` |   | `true`
`source.service` | Source service | `string()` |   | `true`
`source` | Origin of ledger | `object()` |   | `true`
`usage.quantity` | Usage quantity | `integer()` |   | `true`
`usage.type` | Usage type | `string()` |   | `true`
`usage.unit` | Usage unit | `string()` |   | `true`
`usage` | Usage for ledger | `object()` |   | `true`

## Get Available Ledgers

List available ledger sources from the account's reseller.

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/available

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/available
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "name": "per-minute-voip",
            "friendly_name": "Per Minute VoIP",
            "markup_type": [
                "percentage"
            ]
        }
    ],
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}"
}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ledgers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "mobile_data": {
            "amount": -10.5,
            "usage": {
                "quantity": 1000,
                "type": "debit",
                "unit": "MB"
            }
        },
        "per-minute-voip": {
            "amount": -54.7404,
            "usage": {
                "quantity": 14520,
                "type": "voice",
                "unit": "sec"
            }
        }
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}"
}
```

## Get Ledger values

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

## Get Ledger document

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

## Credit / Debit

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
