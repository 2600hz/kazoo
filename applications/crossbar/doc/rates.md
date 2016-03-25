### Rates

#### About Rates

Rates determine what is charged when a call is billed per-minute. This is only available to superduper admins.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`carrier` | Friendly name for the carrier providing this rate | `string` |   | `false`
`description` | Friendly description of the rate | `string` |   | `false`
`internal_rate_cost` | The per-min rate charged by the upstream provider | `number` |   | `false`
`iso_country_code` | Country code this rate applies to | `string` |   | `false`
`options` | List of options this rate is good for, to be matched against a customer's options | `array(string)` |   | `false`
`options.[]` |   | `string` |   | `false`
`prefix` | E.164 prefix (ignoring the +) | `integer` |   | `true`
`rate_cost` | The per-min rate charged to the downstream customer | `number` |   | `true`
`rate_increment` | The time slice, in seconds, to bill in. | `integer` | `60` | `false`
`rate_minimum` | The minimum time slice, in seconds to bill a call | `integer` | `60` | `false`
`rate_name` | Friendly name of the rate | `string` |   | `false`
`rate_nocharge_time` | If the call duration is shorter than this threshold, the call is not billed | `integer` | `0` | `false`
`rate_surcharge` | The upfront cost of connecting the call | `number` | `0` | `false`
`routes` | List of regexs that match valid DIDs for this rate | `array(string)` |   | `false`
`routes.[]` |   | `string` |   | `false`
`weight` | Ordering against other rates, 1 being most preferred, 100 being least preferred | `integer` |   | `false`

#### Fetch available rates

> GET /v2/accounts/{ACCOUNT_ID}/rates

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates
```

#### Upload a ratedeck CSV

For bulk uploading. CSV rows can be formatted in the following ways:

* `Prefix, ISO, Desc, Rate`
* `Prefix, ISO, Desc, InternalRate, Rate`
* `Prefix, ISO, Desc, Surcharge, InternalRate, Rate`
* `Prefix, ISO, Desc, InternalSurcharge, Surcharge, InternalRate, Rate`
* `Prefix, ISO, Desc, InternalSurcharge, Surcharge, Internal_rate, Rate, Routes, RateIncrement, RateMinimum, Direction`

A US-1 row might look like:

`1, "US-1", "US default rate", 0.01`

This API will return an HTTP 202 and process the CSV in a background process.

> POST /v2/accounts/{ACCOUNT_ID}/rates

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: text/csv" \
    --data-binary @/path/to/rates.csv \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates
{
    "auth_token": "{AUTH_TOKEN}",
    "data":""attempting to insert rates from the uploaded document",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/rates

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates/{RATE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/rates/number/{PHONENUMBER}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rates/number/{PHONENUMBER}
```
