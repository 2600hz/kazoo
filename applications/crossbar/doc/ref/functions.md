# Functions

## About Functions

#### Schema

Functions that javascript function to process a callflow document.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | Is the function enabled and running | `boolean()` | `true` | `false` | `supported`
`function_js` | Serverless function in Javascript | `string()` |   | `true` | `supported`
`language_pack` | The language and version are required for the function runtime. | `string()` | `javascript` | `false` | `supported`
`name` | A descriptive name for the function. | `string()` |   | `true` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/functions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/functions

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/functions/samples

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/samples
```

