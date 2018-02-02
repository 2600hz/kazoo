# Notifications

## About Notifications

#### Schema

Notifications templates



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bcc.email_addresses.[]` |   | `string()` |   | `false` |  
`bcc.email_addresses` | BCC Email Addresses | `array(string())` |   | `false` |  
`bcc.type` |   | `string('original' | 'specified' | 'admins')` |   | `false` |  
`bcc` | Bcc email field | `object()` |   | `false` |  
`category` | Category of the template, for grouping purposes | `string(1..)` |   | `false` |  
`cc.email_addresses.[]` |   | `string()` |   | `false` |  
`cc.email_addresses` | CC Email Addresses | `array(string())` |   | `false` |  
`cc.type` |   | `string('original' | 'specified' | 'admins')` |   | `false` |  
`cc` | CC email field | `object()` |   | `false` |  
`enabled` | Enable notification | `boolean()` | `true` | `false` |  
`friendly_name` | Friendly name of the template | `string(1..)` |   | `false` |  
`from` | From: email address | `string()` |   | `true` |  
`macros` |   | `object()` | `{}` | `false` |  
`reply_to` | Reply-To: email address | `string()` |   | `false` |  
`subject` | Email subject | `string(1..200)` |   | `true` |  
`template_charset` |   | `string(1..)` | `utf-8` | `false` |  
`to.email_addresses.[]` |   | `string()` |   | `false` |  
`to.email_addresses` |   | `array(string())` |   | `false` |  
`to.type` |   | `string('original' | 'specified' | 'admins')` |   | `false` |  
`to` | To email field | `object()` |   | `true` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/notifications/customer_update/message

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/customer_update/message
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog/{SMTP_LOG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog/{SMTP_LOG_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/preview

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/preview
```

