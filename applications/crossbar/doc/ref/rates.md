### Rates

#### About Rates

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`carrier` | Friendly name for the carrier providing this rate | `string` |   | `false`
`description` | Friendly description of the rate | `string` |   | `false`
`internal_rate_cost` | The per-min rate charged by the upstream provider | `number` |   | `false`
`iso_country_code` | Country code this rate applies to | `string` |   | `false`
`options` | List of options this rate is good for, to be matched against a customer's options | `array` |   | `false`
`options.[]` |   | `string` |   | `false`
`prefix` | E.164 prefix (ignoring the +) | `integer` |   | `true`
`rate_cost` | The per-min rate charged to the downstream customer | `number` |   | `true`
`rate_increment` | The time slice, in seconds, to bill in. | `integer` | `60` | `false`
`rate_minimum` | The minimum time slice, in seconds to bill a call | `integer` | `60` | `false`
`rate_name` | Friendly name of the rate | `string` |   | `false`
`rate_nocharge_time` | If the call duration is shorter than this threshold, the call is not billed | `integer` | `0` | `false`
`rate_surcharge` | The upfront cost of connecting the call | `number` | `0` | `false`
`routes` | List of regexs that match valid DIDs for this rate | `array` |   | `false`
`routes.[]` |   | `string` |   | `false`
`weight` | Ordering against other rates, 1 being most preferred, 100 being least preferred | `integer` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/rates

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/rates

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/rates

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/rates/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/rates/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/rates/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/rates/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/rates/number/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/rates/number/{ID}
```

