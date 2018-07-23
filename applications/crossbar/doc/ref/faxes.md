# Faxes

## About Faxes

#### Schema

Faxes API allows you to update and access fax jobs for both sending and receiving



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attempts` | The number of attempts made, this will be set by the system and reset automaticly on put/post | `integer()` | `0` | `false` |  
`document.content` | The content provided in the body when fetching for transmission as a post | `string(0..256)` |   | `false` |  
`document.content_type` | The content type header to be used when fetching for transmission as a post | `string()` |   | `false` |  
`document.host` | The host header to be used when fetching for transmission | `string()` |   | `false` |  
`document.method` | The method that should be used to reteive the document | `string('get' | 'post')` | `get` | `false` |  
`document.referer` | The referer header to be used when fetching for transmission | `string()` |   | `false` |  
`document.url` | The url of the fax document | `string()` |   | `true` |  
`document` | Parameters related to the storage of a fax document | `object()` |   | `false` |  
`from_name` | The sender name for the fax | `string()` |   | `false` |  
`from_number` | The sender number for the fax | `string()` |   | `true` |  
`notifications.email.send_to.[]` |   | `string()` |   | `false` |  
`notifications.email.send_to` | A list or string of email recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.email` | Email notifications | `object()` |   | `false` |  
`notifications.sms.send_to.[]` |   | `string()` |   | `false` |  
`notifications.sms.send_to` | A list or string of sms recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.sms` | SMS notifications | `object()` |   | `false` |  
`notifications` | Status notifications | `object()` |   | `false` |  
`retries` | The number of times to retry | `integer()` | `1` | `false` |  
`to_name` | The recipient name for the fax | `string()` |   | `false` |  
`to_number` | The recipient number for the fax | `string()` |   | `true` |  
`tx_result.error_message` | A description of any error that occurred | `string()` | "" | `false` |  
`tx_result.fax_bad_rows` | The number of bad rows | `integer()` | `0` | `false` |  
`tx_result.fax_error_correction` | True if fax error correction was used | `boolean()` | `false` | `false` |  
`tx_result.fax_receiver_id` | The receiver id reported by the remote fax device | `string()` | "" | `false` |  
`tx_result.fax_speed` | The speed (Baud-Rate) achieved during transmission | `integer()` | `0` | `false` |  
`tx_result.pages_sent` | The number of pages transmitted | `integer()` | `0` | `false` |  
`tx_result.success` | True if the fax transmission was successful | `boolean()` | `false` | `false` |  
`tx_result.time_elapsed` | The amount of time from submition to completion | `integer()` | `0` | `false` |  
`tx_result` | The result of a transmission attempt | `object()` |   | `false` |  



## Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxes

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outgoing

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxes/outgoing

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/smtplog

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/smtplog
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{FAX_JOB_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{FAX_JOB_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming/{FAX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming/{FAX_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/smtplog/{ATTEMPT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/smtplog/{ATTEMPT_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment
```

