# Rates

## About Rates

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



## Fetch

> GET /v2/rates

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

## Create

> PUT /v2/rates

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

## Change

> POST /v2/rates

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates
```

## Fetch

> GET /v2/rates/{RATE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

## Change

> POST /v2/rates/{RATE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

## Patch

> PATCH /v2/rates/{RATE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

## Remove

> DELETE /v2/rates/{RATE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/{RATE_ID}
```

## Fetch

> GET /v2/rates/number/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/rates/number/{PHONE_NUMBER}
```

