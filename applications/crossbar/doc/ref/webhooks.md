### Webhooks

#### About Webhooks

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`custom_data` | These properties will be added to the event and will overwrite existing values. | `object` |   | `false`
`enabled` | Is the webhook enabled and running | `boolean` | `true` | `false`
`hook` | The trigger event for a request being made to 'callback_uri'. | `string` |   | `true`
`http_verb` | What HTTP method to use when contacting the server | `string` | `post` | `false`
`name` | A friendly name for the webhook | `string` |   | `true`
`retries` | Retry the request this many times (if it fails) | `integer` | `2` | `false`
`uri` | The 3rd party URI to call out to an event | `string` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/webhooks

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/webhooks

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/webhooks

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/webhooks/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/webhooks/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/webhooks/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/webhooks/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/webhooks/attempts

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks/attempts
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/webhooks/{ID}/attempts

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/webhooks/{ID}/attempts
```

