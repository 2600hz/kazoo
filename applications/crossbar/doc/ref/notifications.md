### Notifications

#### About Notifications

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bcc` | Bcc email field | `object` |   | `false`
`bcc.email_addresses` |  | `array()` |   | `false`
`bcc.type` |  | `string('original', 'specified', 'admins')` |   | `false`
`category` | Category of the template, for grouping purposes | `string(1..)` |   | `false`
`cc` | CC email field | `object` |   | `false`
`cc.email_addresses` |  | `array(string)` |   | `false`
`cc.email_addresses.[]` |   | `string` |   | `false`
`cc.type` |  | `string('original', 'specified', 'admins')` |   | `false`
`enabled` | Enable notification | `boolean` | `true` | `false`
`friendly_name` | Friendly name of the template | `string(1..)` |   | `false`
`from` | From: email address | `string` |   | `true`
`macros` |  | `object` | `{}` | `false`
`reply_to` | Reply-To: email address | `string` |   | `false`
`subject` | Email subject | `string(1..200)` |   | `true`
`template_charset` |  | `string(1..)` | `utf-8` | `false`
`to` | To email field | `object` |   | `true`
`to.email_addresses` |  | `array(string)` |   | `false`
`to.email_addresses.[]` |   | `string` |   | `false`
`to.type` |  | `string('original', 'specified', 'admins')` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/notifications

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/notifications/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog/{_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog/{_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{ID}/preview

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{ID}/preview
```

