### Sms

#### About Sms

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`body` | text message | `string` |   | `true`
`from` | caller-id-number, taken from user if absent | `string` |   | `false`
`scheduled` | The timestamp to start delivering the message | `integer` |   | `false`
`to` | callee-id-number | `string` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/sms

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/sms
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/sms

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/sms
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/sms/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/sms/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/sms/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/sms/{ID}
```

