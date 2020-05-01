# Directories

## About Directories

#### Schema

Allow a caller to search for a user/device by name instead of extension/DID



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`confirm_match` | When one match is found, require caller to confirm the match before connecting | `boolean()` | `true` | `false` | `supported`
`flags.[]` |   | `string()` |   | `false` | `supported`
`flags` | Flags set by external applications | `array(string())` |   | `false` | `supported`
`max_dtmf` | Cap the number of DTMF characters collected from a caller, 0 for unlimited | `integer()` | `0` | `false` | `supported`
`min_dtmf` | How many DTMF characters to collect from a caller before processing the directory | `integer()` | `3` | `false` | `supported`
`name` | The name of the directory | `string(1..)` |   | `true` | `supported`
`search_fields` | What user fields to search DTMF matches on | `string('first_name' | 'last_name' | 'both')` |   | `false` |  
`sort_by` | What field to sort on in matching documents when a caller enters characters | `string('first_name' | 'last_name')` | `last_name` | `false` | `supported`
`users.[]` |   | `string()` |   | `false` | `supported`
`users` | The list of users associated with this directory | `array(string())` | `[]` | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/directories

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/directories

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/directories/{DIRECTORY_ID}
```

