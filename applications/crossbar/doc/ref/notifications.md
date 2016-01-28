### Notifications

#### About Notifications

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bcc` | Bcc email field | `object` |   | `false`
`bcc.email_addresses` |  | `array` |   | `false`
`bcc.type` |  | `string` |   | `false`
`category` | Category of the template, for grouping purposes | `string` |   | `false`
`cc` | CC email field | `object` |   | `false`
`cc.email_addresses` |  | `array` |   | `false`
`cc.email_addresses.[]` |   | `string` |   | `false`
`cc.type` |  | `string` |   | `false`
`enabled` | Enable notification | `boolean` | `true` | `false`
`friendly_name` | Friendly name of the template | `string` |   | `false`
`from` | From: email address | `string` |   | `true`
`macros` |  | `object` | `{}` | `false`
`reply_to` | Reply-To: email address | `string` |   | `false`
`subject` | Email subject | `string` |   | `true`
`template_charset` |  | `string` | `utf-8` | `false`
`to` | To email field | `object` |   | `true`
`to.email_addresses` |  | `array` |   | `false`
`to.email_addresses.[]` |   | `string` |   | `false`
`to.type` |  | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/notifications

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/notifications

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/notifications/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/notifications/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/notifications/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/notifications/smtplog

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications/smtplog
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/notifications/smtplog/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications/smtplog/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/notifications/{ID}/preview

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/notifications/{ID}/preview
```

