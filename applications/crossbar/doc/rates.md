# Call Rates

## About Rates

System operators can create multiple ratedecks and assign those ratedecks to their accounts or sub-accounts via service plans.

If no ratedeck has been assigned, the system ratedeck will be used (backwards-compatible operation).

Flow is:

1. System admin creates a ratedeck CSV and uploads it using the `tasks` API endpoint.
  a. Optionally assign a `ratedeck_name` to each row to add rates to different ratedeck databases
2. Create a service plan for ratedecks
  a. Add the service plan to account(s)
3. When `{ACCOUNT_ID}` has a rate-able call, Kazoo `hotornot` application  will lookup what ratedeck database to use
  a. If using the trie algorithm, `hotornot` will find the PID with that ratedeck's trie and query it
  b. Otherwise, use the view of the ratedeck database to query for rates

#### Schema

Defines a rate for a given prefix



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account_id` | Reseller's account ID | `string()` |   | `false` |  
`caller_id_numbers` | String of caller id prefixes separated by ':' | `string()` |   | `false` |  
`carrier` | Friendly name for the carrier providing this rate | `string()` |   | `false` |  
`description` | Friendly description of the rate | `string()` |   | `false` |  
`direction.[]` |   | `string('inbound' | 'outbound')` |   | `false` |  
`direction` | Apply this rate based on the direction of the call (relative to FreeSWITCH) | `array(string('inbound' | 'outbound'))` |   | `false` |  
`internal_rate_cost` | The per-min rate charged by the upstream provider | `number()` |   | `false` |  
`iso_country_code` | Country code this rate applies to | `string()` |   | `false` |  
`options.[]` |   | `string()` |   | `false` |  
`options` | List of options this rate is good for, to be matched against a customer's options | `array(string())` |   | `false` |  
`prefix` | E.164 prefix (ignoring the +) | `integer()` |   | `true` |  
`rate_cost` | The per-min rate charged to the downstream customer | `number()` |   | `true` |  
`rate_increment` | The time slice, in seconds, to bill in. | `integer()` |   | `false` |  
`rate_minimum` | The minimum time slice, in seconds to bill a call | `integer()` |   | `false` |  
`rate_name` | Friendly name of the rate | `string()` |   | `false` |  
`rate_nocharge_time` | If the call duration is shorter than this threshold (seconds), the call is not billed | `integer()` |   | `false` |  
`rate_suffix` | Suffix applied to rate name | `string()` |   | `false` |  
`rate_surcharge` | The upfront cost of connecting the call | `number()` |   | `false` |  
`rate_version` | Rate version | `string()` |   | `false` |  
`ratedeck_id` | ID of the ratedeck this rate belongs to | `string()` |   | `false` |  
`routes.[]` |   | `string()` |   | `false` |  
`routes` | List of regexps that match valid DIDs for this rate | `array(string())` |   | `false` |  
`weight` | Ordering against other rates, 1 being most preferred, 100 being least preferred | `integer()` |   | `false` |  



## List All Rates

> GET /v2/rates

```shell
curl -v -X GET \
    -H "Accept: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "cost": 0.1,
            "description": "Default US Rate",
            "id":"{RATE_ID}",
            "prefix": "1",
            "surcharge": 0
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

Switch the `Accept` header to `text/csv` to get the page as a CSV.

### List rates matching a prefix

If the ratedeck is huge, rate candidates for particular prefix/number could be retrieved.

For example: a prefix of `1256` will list rates with the prefixes of `1256`, `125`, `12`, and `1`.

> GET /v2/rates?prefix={PREFIX}

```shell
curl -v -X GET \
    -H "Accept: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates?prefix=12223334444
```

```json
{"page_size":1
 ,"data":[{"direction":["inbound"],"prefix":1222,"rate_cost":1.0,"ratedeck_id":"custom","routes":[""],"rate_name":"inbound_1222","rate_suffix":"","rate_surcharge":0.0,"weight":40,"id":"XX-1222"}]
 ,"revision":"{REVISION}"
 ,"timestamp":"{TIMESTAMP}"
 ,"version":"{VERSION}"
 ,"node":"{NODE}"
 ,"request_id":"{REQUEST_ID}"
 ,"status":"success"
 ,"auth_token":"{AUTH_TOKEN}"
}
```

## Upload a Ratedeck CSV

Uploading CSVs has moved to using the ['tasks'](tasks.md) API, which provides a more generic interface. See the [rates task documentation](/applications/tasks/doc/rates.md) for more details on uploading rates.

### Deprecated version

> POST /v2/rates

For bulk uploading. CSV rows can be formatted in the following ways:

* `Prefix, ISO, Desc, Rate`
* `Prefix, ISO, Desc, InternalRate, Rate`
* `Prefix, ISO, Desc, Surcharge, InternalRate, Rate`
* `Prefix, ISO, Desc, InternalSurcharge, Surcharge, InternalRate, Rate`
* `Prefix, ISO, Desc, InternalSurcharge, Surcharge, Internal_rate, Rate, Routes, RateIncrement, RateMinimum, Direction`

A US-1 row might look like:

`1, "US-1", "US default rate", 0.01`

This API will return an HTTP 202 and process the CSV in a background process.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: text/csv" \
    --data-binary @/path/to/rates.csv \
    http://{SERVER}:8000/v2/rates
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data":"attempting to insert rates from the uploaded document",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

### Create a new rate

> PUT /v2/rates

The `routes` key will be populated for you, using the `prefix`, unless you specify the `routes` list here.

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{
        "prefix":"1",
        "iso_country_code": "US",
        "description": "Default US Rate",
        "rate_cost": 0.1
        }}' \
    http://{SERVER}:8000/v2/rates
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "description": "Default US Rate",
        "id": "561d9c4c75950235d5565d138752452c",
        "iso_country_code": "US",
        "prefix": "1",
        "rate_cost": 0.1,
        "rate_increment": 60,
        "rate_minimum": 60,
        "rate_nocharge_time": 0,
        "rate_surcharge": 0,
        "routes": [
            "^\\+?1.+$"
        ]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Remove a rate

> DELETE /v2/rates/{RATE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

```json
{
    "auth_token":"{AUTH_TOKEN}",
    "data": {
        "description": "Default US Rate",
        "id": "{RATE_ID}",
        "iso_country_code": "US",
        "prefix": "1",
        "rate_cost": 0.1,
        "rate_increment": 60,
        "rate_minimum": 60,
        "rate_nocharge_time": 0,
        "rate_surcharge": 0
    },
    "request_id":"{REQUEST_ID}",
    "revision":"{REVISION}",
    "status":"success"
}
```

## Fetch a rate

> GET /v2/rates/{RATE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "description": "Default US Rate",
        "id": "{RATE_ID}",
        "iso_country_code": "US",
        "prefix": "1",
        "rate_cost": 0.1,
        "rate_increment": 60,
        "rate_minimum": 60,
        "rate_nocharge_time": 0,
        "rate_surcharge": 0,
        "routes": [
            "^\\+?1.+$"
        ]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Patch a rate's properties

> PATCH /v2/rates/{RATE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"description": "Default North America Rate"}}' \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "description": "Default North America Rate",
        "id": "{RATE_ID}",
        "iso_country_code": "US",
        "prefix": "1",
        "rate_cost": 0.1,
        "rate_increment": 60,
        "rate_minimum": 60,
        "rate_nocharge_time": 0,
        "rate_surcharge": 0,
        "routes": [
            "^\\+?1.+$"
        ]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Change a rate doc

> POST /v2/rates/{RATE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{
        "description": "Default North America Rate",
        "iso_country_code": "US",
        "prefix": "1",
        "rate_cost": 0.1,
        "rate_increment": 60,
        "rate_minimum": 60,
        "rate_nocharge_time": 0,
        "rate_surcharge": 0,
        "routes": ["^\\+?1.+$"]
        }}'
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "description": "Default North America Rate",
        "id": "{RATE_ID}",
        "iso_country_code": "US",
        "prefix": "1",
        "rate_cost": 0.1,
        "rate_increment": 60,
        "rate_minimum": 60,
        "rate_nocharge_time": 0,
        "rate_surcharge": 0,
        "routes": [
            "^\\+?1.+$"
        ]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## List existing ratedecks (superduper_admin only)

> GET /v2/rates/ratedecks

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/ratedecks
```

## Rate a phone number

This API requires that the backend app `hotornot` is running.

> GET /v2/rates/number/{PHONE_NUMBER}

The `{PHONE_NUMBER}` must be reconcilable (see your `reconcile_regex` for that criteria).

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/number/{PHONE_NUMBER}
```

### Query String Options

The request can take a number of parameters:

Key | Description | Type
--- | ----------- | ----
`ratedeck_id` | If using custom ratedecks, select which ratedeck to rate the number against | `string(32)`
`direction` | What direction is the hypothetical call going | `string('inbound' | 'outbound')`
`caller_id_number` | The caller's number (if rates differentiate based on caller information) | `string()`
`resource_id` | Resource ID for rate filtering | `string(32)`


### Success Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "Base-Cost": 0.1,
        "E164-Number": "+{PHONE_NUMBER}",
        "Prefix": "1",
        "Rate": 0.1,
        "Rate-Description": "Default US Rate",
        "Rate-Increment": "60",
        "Rate-Minimum": "60",
        "Surcharge": 0.0
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

### Error: unrateable phone number

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "message": "No rate found for this number"
    },
    "error": "500",
    "message": "No rate found for this number",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```
