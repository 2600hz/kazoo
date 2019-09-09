# Blacklists

## About Blacklists

#### Schema

Schema for a blacklists



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | Action required apply to call | `string('block' | 'skip_human' | 'ask_human' | 'pass')` | `block` | `false` | `supported`
`enabled` | Blacklist current status | `boolean()` | `true` | `false` | `supported`
`flags.[]` |   | `string()` |   | `false` | `supported`
`flags` | Flags set by external applications | `array(string())` |   | `false` | `supported`
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true` | `supported`
`numbers.name` | Number text description | `string()` |   | `false` | `supported`
`numbers` | Map of caller id number to block or pass | `object()` | `{}` | `false` | `supported`
`owner_id` | The account, user or device ID that 'owns' the blacklist. If owner_id document is missed, then assumed account_id value | `string(32)` |   | `false` | `supported`
`patterns.name` | Regular expressions text description | `string()` |   | `false` | `supported`
`patterns` | Map of regular expressions that blacklist should execute for caller id number to block or pass | `object()` | `{}` | `false` | `supported`
`should_block_anonymous` | Should block Anonymous call | `boolean()` |   | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/blacklists

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/patterns

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/patterns
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/numbers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/numbers
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/patterns/{PATTERN}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/patterns/{PATTERN}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/numbers/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/numbers/{PHONE_NUMBER}
```

