# Webhooks

## About Webhooks

Webhooks allow Kazoo to send HTTP requests to a third-party web server, alerting that server of events occurring within Kazoo. Typically, events would be fired for new calls, when a call is answered, and when a call is finished, though other events will be added in the future.

#### Schema

Web Hooks are subscriptions to allowed events that, when the event occurs, the event data is sent to the uri set in the Web Hook document.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_data` | These properties will be added to the event and will overwrite existing values. | `object()` |   | `false` |  
`enabled` | Is the webhook enabled and running | `boolean()` | `true` | `false` |  
`format` | What Body format to use when sending the webhook. only valid for 'post' & 'put' verbs | `string('form-data' | 'json')` | `form-data` | `false` | `supported`
`hook` | The trigger event for a request being made to 'callback_uri'. | `string()` |   | `true` | `supported`
`http_verb` | What HTTP method to use when contacting the server | `string('get' | 'post' | 'put')` | `post` | `false` | `supported`
`include_internal_legs` | Whether to filter out call legs that are internal to the system (loopback) | `boolean()` | `true` | `false` |  
`include_subaccounts` | Should the webhook be fired for subaccount events. | `boolean()` |   | `false` | `supported`
`name` | A friendly name for the webhook | `string()` |   | `true` | `supported`
`retries` | Retry the request this many times (if it fails) | `integer()` | `2` | `false` | `supported`
`uri` | The 3rd party URI to call out to an event | `string()` |   | `true` | `supported`



## Fetch available webhooks on the system

Depending on the version of the KAZOO system running, the available webhooks may differ. Use this API to query the system for available webhooks.

> GET /v2/webhooks

```shell
curl -v -X GET \
    -H "Content-Type:application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/webhooks
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "description": "Receive notifications when sms is created",
            "id": "sms",
            "name": "SMS"
        },
        {
            "description": "Receive notifications when objects (like JSON document objects) in Kazoo are changed",
            "id": "object",
            "modifiers": {
                "action": {
                    "description": "A list of object actions to handle",
                    "items": [
                        "all",
                        "doc_created",
                        "doc_edited",
                        "doc_deleted"
                    ],
                    "type": "array"
                },
                "type": {
                    "description": "A list of object types to handle",
                    "items": [
                        "account",
                        "all",
                        "callflow",
                        "device",
                        "faxbox",
                        "media",
                        "user",
                        "vmbox",
                        "fax",
                        "mailbox_message",
                        "call_recording"
                    ],
                    "type": "array"
                }
            },
            "name": "Object"
        },
        {
            "description": "Fire a webhook when a notification event is triggered in Kazoo",
            "id": "notifications",
            "modifiers": {
                "type": {
                    "items": {
                        "account_zone_change": {
                            "description": "This event is triggered when an end user requests the home zone of an account is changed",
                            "friendly_name": "Account Zone Change"
                        },
                        "all": {
                            "description": "This event is triggered for any notification events",
                            "friendly_name": "All"
                        },
                        "bill_reminder": {
                            "description": "This event is triggered before a few days before the end of the month toremind account's owners of estimated service plan charges",
                            "friendly_name": "Bill Reminder"
                        },
                        "cf_notification": {
                            "description": "This event is triggered when an customer want send own notification, as example from callflow",
                            "friendly_name": "Customer defined notification"
                        },
                        "cnam_request": {
                            "description": "This event is triggered when an end user would like the CNAM for a number changed",
                            "friendly_name": "CNAM Update"
                        },
                        "customer_update": {
                            "description": "This event is triggered when the customer update API is used to deliver a message to the account",
                            "friendly_name": "Customer Update"
                        },
                        "denied_emergency_bridge": {
                            "description": "This event is triggered when a call to an number classified as emergency fails",
                            "friendly_name": "Emergency Call Failed"
                        },
                        "deregister": {
                            "description": "This event is triggered when a device fails to re-register and the contact expires",
                            "friendly_name": "De-Registration"
                        },
                        "first_occurrence": {
                            "description": "This event is triggered when an end user registers the first device and/or places the first call on an account",
                            "friendly_name": "Account First Occurrence"
                        },
                        "inbound_fax": {
                            "description": "This event is triggered when a fax is successfully received",
                            "friendly_name": "Successful Fax Reception"
                        },
                        "inbound_fax_error": {
                            "description": "This event is triggered when receiving a fax fails",
                            "friendly_name": "Fax Reception Error"
                        },
                        "low_balance": {
                            "description": "This event is triggered when an account is found with a balance below the notification threshold",
                            "friendly_name": "Account Low Balance"
                        },
                        "missed_call": {
                            "description": "This event is triggered when an corresponding missed call action in a callflow is invoked",
                            "friendly_name": "Missed Call"
                        },
                        "new_account": {
                            "description": "This event is triggered when an end user creates a new account",
                            "friendly_name": "New Account"
                        },
                        "new_user": {
                            "description": "This event is triggered when an end user creates a new user",
                            "friendly_name": "New User"
                        },
                        "outbound_fax": {
                            "description": "This event is triggered when a fax is successfully transmitted",
                            "friendly_name": "Successful Fax Transmission"
                        },
                        "outbound_fax_error": {
                            "description": "This event is triggered when transmitting a fax fails",
                            "friendly_name": "Fax Transmission Error"
                        },
                        "outbound_smtp_fax_error": {
                            "description": "This event is triggered when the received email-to-fax email is invalid",
                            "friendly_name": "Invalid Email-to-Fax Email"
                        },
                        "password_recovery": {
                            "description": "This event is triggered when an end user requests a password recovery link",
                            "friendly_name": "Password Recovery"
                        },
                        "port_cancel": {
                            "description": "This event is triggered when a port request is canceled",
                            "friendly_name": "Port Cancel"
                        },
                        "port_comment": {
                            "description": "This event is triggered when a comment is left on a port request",
                            "friendly_name": "Port Comment"
                        },
                        "port_pending": {
                            "description": "This event is triggered when a port request is accepted and submitted to a carrier",
                            "friendly_name": "Port Pending"
                        },
                        "port_rejected": {
                            "description": "This event is triggered when a port request is rejected",
                            "friendly_name": "Port Rejected"
                        },
                        "port_request": {
                            "description": "This event is triggered when a port is submitted for processing",
                            "friendly_name": "Port Request"
                        },
                        "port_scheduled": {
                            "description": "This event is triggered when a port is accepted by a carrier and scheduled",
                            "friendly_name": "Port Scheduled"
                        },
                        "port_unconfirmed": {
                            "description": "This event is triggered when a port is created, prior to submitting",
                            "friendly_name": "Port Unconfirmed"
                        },
                        "ported": {
                            "description": "This event is triggered when a port request for number is completed",
                            "friendly_name": "Ported"
                        },
                        "register": {
                            "description": "This event is triggered when a device registers but is not currently registered",
                            "friendly_name": "Registration"
                        },
                        "service_added": {
                            "description": "This event is triggered when an account's billable quantities change",
                            "friendly_name": "Service Added"
                        },
                        "topup": {
                            "description": "This event is triggered when an account automatic top-up is attempted",
                            "friendly_name": "Automatic Account Top-up"
                        },
                        "transaction": {
                            "description": "This event is triggered when a transaction is attempted",
                            "friendly_name": "Transaction Completed"
                        },
                        "voicemail_full": {
                            "description": "This event is triggered any time an attempt to leave a voicemail message is blocked because the voicemail box is full",
                            "friendly_name": "Voicemail Box Full"
                        },
                        "voicemail_new": {
                            "description": "This event is triggered any time a voicemail message is left",
                            "friendly_name": "New Voicemail Message"
                        },
                        "voicemail_saved": {
                            "description": "This event is triggered any time a voicemail message is saved in the voicemail box 'new' folder",
                            "friendly_name": "Voicemail Message Saved"
                        },
                        "webhook": {
                            "description": "This event is triggered when a corresponding webhook action in a callflow is reached",
                            "friendly_name": "Callflow Webhook Triggered"
                        }
                    },
                    "type": "object"
                }
            },
            "name": "Notifications Webhook"
        },
        {
            "description": "This webhook is triggered when a channel is destroyed, usually as a result of a hangup",
            "id": "channel_destroy",
            "name": "Channel Destroy"
        },
        {
            "description": "This webhook is triggered when a new channel is created",
            "id": "channel_create",
            "name": "Channel Create"
        },
        {
            "description": "This webhook is triggered when two channels are bridged together, such as two users/devices connected together",
            "id": "channel_bridge",
            "name": "Channel Bridge"
        },
        {
            "description": "This webhook is triggered when a channel establishes two-way audio, such as a voicemail box or the called party answering",
            "id": "channel_answer",
            "name": "Channel Answer"
        },
        {
            "description": "Events when calls get parked/retrieved",
            "id": "parking",
            "name": "Call Parking"
        }
    ],
   "page_size": 8,
   "request_id": "{REQUEST_ID}",
   "revision": "{REVISION}",
   "status": "success"
}
```

## Get sample payloads of all webhook events

> GET /v2/webhooks/samples

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/webhooks/samples
```

### Example

**Request:**

```shell
curl -H 'Content-Type: application/json' 'http://{SERVER}:8000/v2/webhooks/samples'
```

**Response:**

```json
{
  "data": [
    "webhooks_channel_answer",
    "webhooks_channel_bridge",
    "webhooks_channel_create",
    "webhooks_channel_destroy",
    "webhooks_notifications",
    "webhooks_object",
    "webhooks_parking"
  ],
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success"
}
```

## Get sample payloads of a webhook event

> GET /v2/webhooks/samples/{SAMPLE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/webhooks/samples/{SAMPLE_ID}
```

You can use regular [Crossbar query string filters](filters.md) to narrow down the samples, for example:

```shell
curl -H 'Content-Type: application/json' 'http://{SERVER}:8000/v2/webhooks/samples/webhook_notifications?filter_event_name=missed_call'
curl -H 'Content-Type: application/json' 'http://{SERVER}:8000/v2/webhooks/samples/webhook_object?filter_action=doc_created'
```

### Example

**Request:**

```shell
curl -s -H 'Content-Type: application/json' 'http://{SERVER}:8000/v2/webhooks/samples/webhooks_parking'
```

**Response:**

```json
{
  "page_size": 1,
  "data": [
    {
      "account_id": "5a2d994fbae69b1d6b01eb9f0e7dfe62",
      "call_id": "OWU4NzEwOTgyZWNiMjM0MzI0NjRkZDc4MWVmMjEyOWI",
      "callee_id_name": "Test Name",
      "callee_id_number": "5355543456",
      "caller_id_Number": "+15555432345",
      "caller_id_name": "Superman",
      "event_name": "PARK_PARKED",
      "parking_slot": 1
    }
  ],
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success"
}
```

## List webhooks

> GET /v2/accounts/{ACCOUNT_ID}/webhooks

Any webhooks with *disable_reason* in the summary has been auto-disabled.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

## Create webhook

> PUT /v2/accounts/{ACCOUNT_ID}/webhooks

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
        "name": "New Calls",
        "uri": "http://my.{SERVER}/calls/new.php",
        "http_verb": "post",
        "hook": "channel_create",
        "retries":3
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

## Get details of the webhook

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Edit webhook

> POST /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
        "name": "New Calls",
        "uri": "http://my.{SERVER}/calls/new_calls.php",
        "http_verb": "post",
        "hook": "channel_create",
        "retries": 3
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Patch webhook

> PATCH /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

You can also patch an existing webhook:

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"enabled":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## Delete a webhook

> DELETE /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```

## List Webhook Attempts

Webhooks tracks attempts to send the hook payloads to your URIs. You can get a listing of the more recent attempts to help debug what went wrong.

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/attempts
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "client_error": "nxdomain",
            "hook_id": "{HOOK_ID}",
            "reason": "kazoo http client error",
            "result": "failure",
            "retries left": 2,
            "timestamp": 63590996563
        },
        {
            "hook_id": "{HOOK_ID}",
            "result": "success",
            "timestamp": 63590996562
        }
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "status": "success"
    }
```

## List attempts for a specific attempt

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts
```

## Re-enable auto-disabled hooks in bulk

Webhooks will auto-disable failing hooks (if Kazoo can't reach your server, or you take too long to respond with `200 OK`, for instance). Especially if you're a reseller with webhooks in your client accounts, it can be tedious to have to iterate through all your accounts and re-enable each hook. Fortunately, you can perform this bulk-enable action against an account or an account and its descendants.

Enable an account's hooks

> PATCH /v2/accounts/{ACCOUNT_ID}/webhooks

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"re-enable":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```

Enable an account's and descendant accounts' hooks

> PATCH /v2/accounts/{ACCOUNT_ID}/descendants/webhooks

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"re-enable":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants/webhooks
```

## Hook Payload

Here's what you can expect to receive when a webhook fires to your server:

### Base Payload

* hook_event: The type of hook being fired
* call_direction: "inbound" or "outbound", relative to Kazoo
* timestamp: Gregorian timestamp of the event
* account_id: ID of the account generating the event
* request: SIP Request
* to: SIP To
* from: SIP From
* call_id: SIP Call ID
* other_leg_call_id: If bridged, the Call ID of the other leg
* caller_id_name: Caller ID Name
* caller_id_number: Caller ID Number
* callee_id_name: Callee Name
* callee_id_number: Callee Number

Most of these fields should be present on all payloads.

### Hook Specific fields

* channel_create
    * hook_event: channel_create
* channel_answer
    * hook_event: channel_answer
* channel_destroy
    * hook_event: channel_destroy
    * hangup_cause: SIP Hangup Cause (NORMAL_CLEARING, ORIGINATOR_CANCEL, etc)
    * hangup_code: SIP Hangup Code (404, 503, etc)
* object
    * hook_event: object
    * action: doc_created, doc_updated, doc_deleted
    * type: user, vmbox, callflow, account, device, faxbox, media


## Hook Specific Custom Data

To restrict the kind of document or the action or both. You can set the custom data to:

```json
{"name":"User edited webhook"
 ,"hook":"object"
 ,"custom_data":{
    "type": "user",
    "action": "doc_edited"
 }
 ,"uri":"https://..."
}
```

Both `type` and `action` will accept `"all"` to avoid needing to create a hook per action/type combinations.
