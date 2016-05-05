## Faxes

### Fax Subsystem Overview

The Faxes API exposes lots of ways to send, receive, track and manage faxes.

As a general concept, faxes are either considered inbound or outbound faxes. In addition:
* API calls with the term "incoming" are used for tracking faxes currently in the process of being received
* API calls with the term "inbox" are used for managing faxes which have already been received
* API calls with the term "outgoing" are used for tracking faxes currently in the process of being sent
* API calls with the term "outbox" are used for managing faxes which have already been sent


#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attempts` | The number of attempts made, this will be set by the system and reset automaticly on put/post | `integer` | `0` | `false`
`document` | Parameters related to the storage of a fax document | `object` |   | `false`
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

#### Processing States

State | Description
----- | -----------
`attaching_files` | A fax job was submitted via the api (with a multipart/related content type) or smtp and we are in the process of attaching the files to the fax job.
`pending` | Fax waiting to be picked up by the fax sending job
`failed` | If we can't retrieve the fax document via a requests URL, the state will be "failed" and the error text will contain "could not retrieve file, http response <XXX>"
`processing` | Faxes that are actively picked up by the fax worker and are being processed
`completed` | Faxes that are finished sending
`failed` | Faxes that did not successfully send after all allotted retries are in state "failed". We pass-thru the FreeSWITCH error code in this case.

### Sending Outbound Faxes

This section details APIs for manipulating job processing of outgoing faxes.

#### Create an outgoing fax

There are two methods for creating an outgoing fax - they differ in how you attach the fax file for processing.

In the first method, you can create a fax document that includes a URL which contains the fax document to send. The fax document is fetched by the `fax_jobs` worker and distributed to `fax_worker` processes. You can fetch the status of the created job using the `faxes/outgoing/{FAX_ID}` API.

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

In the second method, you can use a single PUT request and send a multipart content-type to attach both the JSON metadata about the fax transmission and the document itself, in a single request. This avoids needing to have an external storage location for storing fax attachments prior to processing. This is a good solution for portals that upload documents.

```curl
curl -v -X PUT \
     -H "Content-Type: multipart/mixed" \
     -F "content=@fax.json; type=application/json" \
     -F "content=@fax.pdf; type=application/pdf" \
     -H 'X-Auth-Token: {AUTH_TOKEN}' \
     http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes
```

#### Create an outgoing fax (Alias)

This is identical to the `PUT /faxes` above.

> PUT /v2/accounts/{ACCOUNT_ID}/faxes/outgoing

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
```

#### Fetch outgoing faxes and their statuses

This API retrieves a listing of all outgoing faxes. Use the "id" to fetch details about a particular job. Results will contain a listing of both API- and SMTP (email) - initiated outbound faxes.

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outgoing

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "created": 63626410973,
            "from": "18884732963",
            "id": "{FAXJOB_ID}",
            "status": "pending",
            "to": "18884732963"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "e7dc82251f713694d4ddd5a95bf3701c",
    "start_key": [
        "{START_KEY}"
    ],
    "status": "success"
}
```

#### Fetch details of a queued outgoing fax job

Get all the details about a fax that is in the outgoing queue.

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

### Managing Past Outbound Faxes

#### Fetch all previously sent faxes in the outbox folder

This API retrieves a listing of all outgoing faxes which have already been sent or attempted and are no longer in queue. Results will contain a listing of both API- and SMTP (email) - initiated outbound faxes.

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox
```

#### Fetch a fax from the outbox folder

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}
```

#### Fetch the fax payload

> GET /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment
```


#### Fetch logs related to outbound faxes submitted via email

If a fax job was queued or attempted to be queued as the result of an inbound email, the SMTP log for that fax can be retrieved via this API. This is also useful for helping debug problems with inbound faxes, such as when the domain matched an account for an inbound fax, but not a specific faxbox, and thus failed to process.

> GET /v2/accounts/{ACCOUNT_ID}/faxes/smtplog

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/smtplog
```

#### Fetch a specific log related to email

> GET /v2/accounts/{ACCOUNT_ID}/faxes/smtplog/{ATTEMPT_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/smtplog/{ATTEMPT_ID}
```


#### Remove a fax from the outbox folder

This API allows you to delete an old fax message. For privacy reasons, this may be useful if you wish to remove all evidence of a previously sent outbound fax.

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}
```

#### Remove the fax payload

In some cases, you may wish to remove the document from a fax (usually for privacy reasons) but keep evidence that the fax transmission occurred. This will remove attachments but not the metadata from a sent fax.


> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/outbox/{FAX_ID}/attachment
```

### Managing Past Inbound Faxes

#### Fetch all faxes in the inbox folder

Retrieve a list of faxes that have previously been received.

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox
```

#### Fetch a fax from the inbox folder

This API retrieves all metadata about a particular fax for which you have the fax ID.

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
```

#### Fetch the fax payload

This API retrieves the fax document / attachments for a particular inbound fax for which you have the fax ID.

> GET /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment
```

#### Remove a fax from the inbox folder

This API allows you to delete an old fax message. For privacy reasons, this may be useful if you wish to remove all evidence of a previously received inbound fax.

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}
```

#### Remove the fax payload

In some cases, you may wish to remove the document from a fax (usually for privacy reasons) but keep evidence that the fax receipt occurred. This will remove attachments but not the metadata from a received fax. Useful after you've done post-processing on a fax externally.

> DELETE /v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/inbox/{FAX_ID}/attachment
```

## APIs under active development

### Receiving Inbound Faxes

#### Fetch

Retrieve a list of faxes that are currently being received or attempted to be received.
NOTE: THIS FUNCTION DOES NOT WORK YET AS OF THE WRITING OF THIS DOCUMENT. We'll update this doc once this function is complete. Ticket #

> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming
```

#### Fetch an incoming fax job


> GET /v2/accounts/{ACCOUNT_ID}/faxes/incoming/{FAX_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxes/incoming/{FAX_ID}
```
