### Rates

#### About Rates

#### Schema

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


#### Fetch

> GET /v2/rates

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

#### Change

> POST /v2/rates

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

#### Create

> PUT /v2/rates

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

#### Remove

> DELETE /v2/rates/{RATE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

#### Fetch

> GET /v2/rates/{RATE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

#### Patch

> PATCH /v2/rates/{RATE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

#### Change

> POST /v2/rates/{RATE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

#### Fetch

> GET /v2/rates/number/{PHONENUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/number/{PHONENUMBER}
```

