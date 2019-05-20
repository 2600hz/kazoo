# Webhooks

## About Webhooks

#### Schema

Web Hooks are subscriptions to allowed events that, when the event occurs, the event data is sent to the uri set in the Web Hook document.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_data` | These properties will be added to the event and will overwrite existing values. | `object()` |   | `false` |  
`enabled` | Is the webhook enabled and running | `boolean()` | `true` | `false` |  
`hook` | The trigger event for a request being made to 'callback_uri'. | `string()` |   | `true` | `supported`
`http_verb` | What HTTP method to use when contacting the server | `string('get' | 'post')` | `post` | `false` | `supported`
`include_internal_legs` | Whether to filter out call legs that are internal to the system (loopback) | `boolean()` | `true` | `false` |  
`include_subaccounts` | Should the webhook be fired for subaccount events. | `boolean()` |   | `false` | `supported`
`name` | A friendly name for the webhook | `string()` |   | `true` | `supported`
`retries` | Retry the request this many times (if it fails) | `integer()` | `2` | `false` | `supported`
`uri` | The 3rd party URI to call out to an event | `string()` |   | `true` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/webhooks

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/webhooks

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/samples

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/samples
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/attempts
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/samples/{SAMPLE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/samples/{SAMPLE_ID}
```

