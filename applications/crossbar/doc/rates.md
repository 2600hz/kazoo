### Rates

#### About Rates

#### Schema

Defines a rate for a given prefix

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`carrier` | Friendly name for the carrier providing this rate | `string` |   | `false`
`description` | Friendly description of the rate | `string` |   | `false`
`direction` | Apply this rate based on the direction of the call (relative to FreeSWITCH) | `array(string('inbound', 'outbound'))` | `["inbound", "outbound"]` | `false`
`direction.[]` |   | `string` |   | `false`
`internal_rate_cost` | The per-min rate charged by the upstream provider | `number` |   | `false`
`iso_country_code` | Country code this rate applies to | `string` |   | `false`
`options` | List of options this rate is good for, to be matched against a customer's options | `array(string)` |   | `false`
`options.[]` |   | `string` |   | `false`
`prefix` | E.164 prefix (ignoring the +) | `integer` |   | `true`
`rate_cost` | The per-min rate charged to the downstream customer | `number` |   | `true`
`rate_increment` | The time slice, in seconds, to bill in. | `integer` | `60` | `false`
`rate_minimum` | The minimum time slice, in seconds to bill a call | `integer` | `60` | `false`
`rate_name` | Friendly name of the rate | `string` |   | `false`
`rate_nocharge_time` | If the call duration is shorter than this threshold (seconds), the call is not billed | `integer` | `0` | `false`
`rate_surcharge` | The upfront cost of connecting the call | `number` | `0` | `false`
`routes` | List of regexs that match valid DIDs for this rate | `array(string)` | `[]` | `false`
`routes.[]` |   | `string` |   | `false`
`weight` | Ordering against other rates, 1 being most preferred, 100 being least preferred | `integer` |   | `false`
`ratedeck_name` | Ratedeck name | `string` |   | `false`
`account_id` | Reseller's account ID | `string` |   | `false`
`rate_version` | Version of rate | `string` |   | `false`




#### Fetch available rates

> GET /v2/rates

```shell
curl -v -X GET \
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

#### Upload a ratedeck CSV

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
    "data":""attempting to insert rates from the uploaded document",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Create a new rate

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

#### Remove a rate

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

#### Fetch a rate

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

#### Patch a rate's properties

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

#### Change a rate doc

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

#### Rate a phone number

This API requires that the backend app `hotornot` is running.

> GET /v2/rates/number/{PHONE_NUMBER}

The `{PHONE_NUMBER}` must be reconcilable (see your `reconcile_regex` for that criteria).

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/number/{PHONE_NUMBER}
```

##### Success

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

##### Error: unrateable phone number

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
