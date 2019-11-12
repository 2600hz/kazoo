# Limits

Configures limit on call consumed for your account.

## About Limits

#### Schema

Limit an account's ability to place concurrent calls using flat rate trunks



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`allow_prepay` | Determines if the account would like to allow per-minute calls if they have no available credit | `boolean()` | `true` | `false` | `supported`
`authz_resource_types.[]` |   | `string()` |   | `false` | `beta`
`authz_resource_types` | List of Resource-Types that will authorize the call; bypassing limit checks and billing | `array(string())` | `[]` | `false` | `beta`
`burst_trunks` | The number of two-way, flat-rate trunks used only if no other trunks are available | `integer()` |   | `false` | `beta`
`calls` | A hard limit for the total number calls | `integer()` |   | `false` | `beta`
`inbound_trunks` | The number of inbound, flat-rate trunks | `integer()` |   | `false` | `supported`
`outbound_trunks` | The number of outbound, flat-rate trunks | `integer()` |   | `false` | `supported`
`resource_consuming_calls` | A hard limit for the number of resource consuming calls | `integer()` |   | `false` | `beta`
`twoway_trunks` | The number of two-way, flat-rate trunks | `integer()` |   | `false` | `beta`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/limits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/limits
```

```json
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 0,
        "id": "limits",
        "allow_prepay": true,
        "outbound_trunks": 5
    },
    "status": "success"
}
```

## Update limits for a given account

> POST /v2/accounts/{ACCOUNT_ID}/limits

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "id": "limits",
        "allow_prepay": true,
        "outbound_trunks": 5
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/limits
```

Reply warns that charges have to be accepted (402):

```
{
    "data": {
        "limits": {
            "inbound_trunks": {
                "category": "limits",
                "item": "inbound_trunks",
                "quantity": 11,
                "rate": 6.9900000000000002132,
                "single_discount": true,
                "single_discount_rate": 0.0,
                "cumulative_discount": 0,
                "cumulative_discount_rate": 0.0
            }
        }
    },
    "error": "402",
    "message": "accept charges",
    "status": "error",
}
```

Re-do the same request, setting `accept_charges` to `true`.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "id": "limits",
        "allow_prepay": true,
        "outbound_trunks": 5,
        "accept_charges": true
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/limits
```

```json
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "id": "limits",
        "allow_prepay": true,
        "outbound_trunks": 5
    },
    "status": "success",
}
```
