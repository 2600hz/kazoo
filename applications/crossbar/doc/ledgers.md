# Ledgers

## About Ledgers

The Ledgers API provides an easy way to see usage like per-minutes or flat-rate and manage your account's credit/debit values.

## Ledgers Schema

#### Schema

ledgers document



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account.id` | Account ID | `string()` |   | `false` |  
`account.name` | Account name | `string()` |   | `false` |  
`account` | Account info | `object()` |   | `false` |  
`amount` | Ledger amount, in currency amount | `number()` |   | `false` |  
`description` | Useful description for ledger | `string()` |   | `false` |  
`metadata` | Metadata for ledger document | `object()` |   | `false` |  
`period.end` | Period end | `integer()` |   | `false` |  
`period.start` | Period start | `integer()` |   | `false` |  
`period` | Period of ledger | `object()` |   | `false` |  
`source.id` | Source ID | `string()` |   | `true` |  
`source.service` | Source service | `string()` |   | `true` |  
`source` | Origin of ledger | `object()` |   | `true` |  
`usage.quantity` | Usage quantity | `integer()` |   | `true` |  
`usage.type` | Usage type | `string()` |   | `true` |  
`usage.unit` | Usage unit | `string()` |   | `true` |  
`usage` | Usage for ledger | `object()` |   | `true` |  



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

## Fetch Ledgers by source

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/{SOURCE_SERVICE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{SOURCE_SERVICE}
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

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/{SOURCE_SERVICE}/{LEDGER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{SOURCE_SERVICE}/{LEDGER_ID}
```

## Fetch Ledger and Account Summary Breakdown

Fetches the account ledger summary as well as a breakdown of ledgers per account for a given YYYYMM.

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/summary/{MODB_SUFFIX}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/summary/{MODB_SUFFIX}
```

### Example

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/summary/201905
```

```json
"data": {
    "summary": {
      "per-minute-voip": {
        "amount": -880.0,
        "usage": {
          "type": "voice",
          "quantity": 232320,
          "unit": "sec"
        }
      },
      "payments": {
        "amount": 251970.82,
        "usage": {
          "type": "debit",
          "quantity": 0,
          "unit": "dollars"
        }
      },
      "prorations": {
        "amount": -1.9258,
        "usage": {
          "quantity": 0
        }
      },
      "rollovers": {
        "amount": 36.102,
        "usage": {
          "quantity": 0
        }
      }
    },
    "breakdown": [
      {
        "account": {
          "id": "de6fd29a54407cfe46e6c9d3828ab0d8",
          "name": "Account A"
        },
        "ledgers": {
          "payments": {
            "amount": -1250000.0,
            "usage": {
              "type": "debit",
              "quantity": 0,
              "unit": "dollars"
            }
          },
          "per-minute-voip": {
            "amount": -385.0,
            "usage": {
              "type": "voice",
              "quantity": 101640,
              "unit": "sec"
            }
          }
        },
        "total": -1250385.0
      },
      {
        "account": {
          "id": "cc580f94d7da53816a94b87b2a1d25f8",
          "name": "Account 1"
        },
        "ledgers": {
          "payments": {
            "amount": 1501970.82,
            "usage": {
              "type": "credit",
              "quantity": 0,
              "unit": "dollars"
            }
          },
          "prorations": {
            "amount": -1.9258,
            "usage": {
              "quantity": 0
            }
          },
          "rollovers": {
            "amount": 36.102,
            "usage": {
              "quantity": 0
            }
          }
        },
        "total": 1500004.9962
      },
      {
        "account": {
          "id": "a71a670531d7dc92d2a4f9fc6774df36",
          "name": "Account B"
        },
        "ledgers": {
          "per-minute-voip": {
            "amount": -495.0,
            "usage": {
              "type": "voice",
              "quantity": 130680,
              "unit": "sec"
            }
          }
        },
        "total": -495.0
      }
    ]
  }
```
