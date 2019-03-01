# Blacklists

## About Blacklists

A blacklist is a map of caller id numbers that can be then apply to the account to block these callers to call the system.

#### Schema

Schema for a blacklists



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true` | `supported`
`numbers` | Map of caller id number to block | `object()` | `{}` | `false` | `supported`
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
