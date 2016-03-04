### Webhooks

#### About Webhooks

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`custom_data` | These properties will be added to the event and will overwrite existing values. | `object` |   | `false`
`enabled` | Is the webhook enabled and running | `boolean` | `true` | `false`
`hook` | The trigger event for a request being made to 'callback_uri'. | `string` |   | `true`
`http_verb` | What HTTP method to use when contacting the server | `string('get', 'post')` | `post` | `false`
`name` | A friendly name for the webhook | `string` |   | `true`
`retries` | Retry the request this many times (if it fails) | `integer` | `2` | `false`
`uri` | The 3rd party URI to call out to an event | `string` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/webhooks

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/webhooks

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/attempts

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/attempts
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts
```

