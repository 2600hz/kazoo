# Faxboxes

## About Faxboxes

Fax boxes are used to receive, send and store incoming or outgoing faxes, allowing for configuration of individual fax virtual machines.

## Schema

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attempts` | The number of attempts made, this will be set by the system and reset automatically on put/post | `integer()` | `0` | `false` |  
`caller_id` | The Caller-ID-Number | `string()` |   | `false` | `supported`
`caller_name` | The Caller-ID-Name | `string()` | `Kazoo Fax Printer` | `false` | `supported`
`custom_smtp_email_address` | custom smtp address | `string()` |   | `false` | `supported`
`fax_header` | The name printed at the top of the fax | `string()` | `Kazoo Fax Printer` | `false` | `supported`
`fax_identity` | The number printed at the top of the fax | `string()` |   | `false` | `supported`
`fax_timezone` | The timezone announced | `string()` |   | `false` | `supported`
`media.fax_option` | Is T.38 Supported? | `boolean()` |   | `false` | `beta`
`media` | The faxbox media parameters | `object()` | `{}` | `false` | `beta`
`name` | A friendly name for the faxbox | `string(1..128)` |   | `true` | `supported`
`notifications.inbound.callback.method` | The http method to use when sending the results | `string('post' | 'put')` |   | `false` |  
`notifications.inbound.callback.type` | The content-type to use when sending the results | `string('json' | 'www-url-form-encoded')` |   | `false` |  
`notifications.inbound.callback.url` | The URL to call back with the results | `string()` |   | `false` |  
`notifications.inbound.callback` | A URL to send results to | `object()` |   | `false` | `beta`
`notifications.inbound.email.send_to.[]` |   | `string()` |   | `false` |  
`notifications.inbound.email.send_to` | A list or string of email recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.inbound.email` | Inbound Email Notifications | `object()` |   | `false` | `supported`
`notifications.inbound.sms.send_to.[]` |   | `string()` |   | `false` |  
`notifications.inbound.sms.send_to` | A list or string of sms recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.inbound.sms` | SMS notifications | `object()` |   | `false` | `beta`
`notifications.inbound` | Inbound Status notifications | `object()` |   | `false` | `supported`
`notifications.outbound.callback.method` | The http method to use when sending the results | `string('post' | 'put')` |   | `false` |  
`notifications.outbound.callback.type` | The content-type to use when sending the results | `string('json' | 'www-url-form-encoded')` |   | `false` |  
`notifications.outbound.callback.url` | The URL to call back with the results | `string()` |   | `false` |  
`notifications.outbound.callback` | A URL to send results to | `object()` |   | `false` | `beta`
`notifications.outbound.email.send_to.[]` |   | `string()` |   | `false` |  
`notifications.outbound.email.send_to` | A list or string of email recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.outbound.email` | Email notifications | `object()` |   | `false` | `supported`
`notifications.outbound.sms.send_to.[]` |   | `string()` |   | `false` |  
`notifications.outbound.sms.send_to` | A list or string of sms recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.outbound.sms` | SMS notifications | `object()` |   | `false` | `beta`
`notifications.outbound` | Outbound Status notifications | `object()` |   | `false` | `supported`
`notifications` | Status notifications | `object()` |   | `false` | `supported`
`retries` | The number of times to retry | `integer()` | `1` | `false` | `supported`
`smtp_permission_list.[]` |   | `string()` |   | `false` | `supported`
`smtp_permission_list` | smtp permission list. accepts regular expressions | `array(string())` | `[]` | `false` | `supported`

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxboxes

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxboxes

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

