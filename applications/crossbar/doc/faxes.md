### Faxes

#### About Faxes


#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attempts` | The number of attempts made, this will be set by the system and reset automaticly on put/post | `integer` | `0` | `false`
`callback` | A URL to send results to | `object` |   | `false`
`callback.method` | The HTTP method used for the callback | `string('post', 'put')` |   | `false`
`callback.type` | The content-type used for the body of the callback | `string('json', 'www-url-form-encoded')` |   | `false`
`callback.url` | The URL to call back with the results | `string` |   | `false`
`document` | Parameters related to the storage of a fax document | `object` | `{}` | `false`
`document.content` | The content provided in the body when fetching for transmission as a post | `string(0..256)` |   | `false`
`document.content_type` | The content type header to be used when fetching for transmission as a post | `string` |   | `false`
`document.host` | The host header to be used when fetching for transmission | `string` |   | `false`
`document.method` | The method that should be used to reteive the document | `string('get', 'post')` | `get` | `false`
`document.referer` | The referer header to be used when fetching for transmission | `string` |   | `false`
`document.url` | The url of the fax document | `string` |   | `false`
`from_name` | The sender name for the fax | `string` |   | `false`
`from_number` | The sender number for the fax | `string` |   | `false`
`notifications` | Status notifications | `object` |   | `false`
`notifications.email` | Email notifications | `object` |   | `false`
`notifications.email.send_to` | A list or string of email recipent(s) | `string, array(string)` |   | `false`
`notifications.sms` | SMS notifications | `object` |   | `false`
`notifications.sms.send_to` | A list or string of sms recipent(s) | `string, array(string)` |   | `false`
`retries` | The number of times to retry | `integer` | `1` | `false`
`to_name` | The recipient name for the fax | `string` |   | `false`
`to_number` | The recipient number for the fax | `string` |   | `false`
`tx_result` | The result of a transmission attempt | `object` | `{}` | `false`
`tx_result.error_message` | A description of any error that occured | `string` | "" | `false`
`tx_result.fax_bad_rows` | The number of bad rows | `integer` | `0` | `false`
`tx_result.fax_error_correction` | True if fax error correction was used | `boolean` | `false` | `false`
`tx_result.fax_receiver_id` | The receiver id reported by the remote fax device | `string` | "" | `false`
`tx_result.fax_speed` | The speed achieved during transmission | `integer` | `0` | `false`
`tx_result.pages_sent` | The number of pages transmitted | `integer` | `0` | `false`
`tx_result.success` | True if the fax transmission was successful | `boolean` | `false` | `false`
`tx_result.time_elapsed` | The amount of time from submition to completion | `integer` | `0` | `false`


#### Create an outgoing fax

Create a fax document that includes where to find the document to send. These are fetched by the `fax_jobs` worker and distributed to `fax_worker` processes. You can fetch the status of the created job using the `faxes/outgoing/{FAX_ID}` path

> PUT /v2/accounts/{ACCOUNT_ID}/faxes

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"document":{"url":"http://myserver.com/fax.pdf","method":"get"},"retries":3,"from_name":"Test Fax","from_number":"18884732963","to_name":"To Name","to_number":"18884732963"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes
{
    "data":{
        "document":{
            "url":"http://myserver.com/fax.pdf",
            "method":"get"
        },
        "retries":3,
        "from_name":"Test Fax",
        "from_number":"18884732963",
        "to_name":"To Name",
        "to_number":"18884732963",
        "attempts":0,
        "tx_result":{
            "error_message":"",
            "fax_bad_rows":0,
            "fax_error_correction":false,
            "fax_receiver_id":""
            ,"fax_speed":0,
            "pages_sent":0,
            "success":false,
            "time_elapsed":0
        },
        "fax_timezone":"undefined",
        "id":"{FAX_JOB_ID}"
    },
    "revision":"1-53e7a67f473fe55d586a3b10dcca3ced",
    "request_id":"{REQUEST_ID}",
    "status":"success",
    "auth_token":"{AUTH_TOKEN}"
}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outgoing

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxes/outgoing

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/smtplog

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/smtplog
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{FAXJOB_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{FAXJOB_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attempts": 0,
        "created": 63626410973,
        "delivered": "undefined",
        "document": {
            "method": "get",
            "url": "http://myserver.com/fax.pdf"
        },
        "fax_timezone": "undefined",
        "from_name": "Test Fax",
        "from_number": "18884732963",
        "id": "{FAXJOB_ID}",
        "retries": 3,
        "status": "pending",
        "to_name": "To Name",
        "to_number": "18884732963",
        "tx_result": {
            "error_message": "",
            "fax_bad_rows": 0,
            "fax_error_correction": false,
            "fax_receiver_id": "",
            "fax_speed": 0,
            "pages_sent": 0,
            "success": false,
            "time_elapsed": 0
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "1-53e7a67f473fe55d586a3b10dcca3ced",
    "status": "success"
}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing/{_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/smtplog/{_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/smtplog/{_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}/attachment

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}/attachment
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}/attachment

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming/{_ID}/attachment
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}/attachment

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}/attachment
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}/attachment

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{_ID}/attachment
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}/attachment

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}/attachment
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}/attachment

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{_ID}/attachment
```
