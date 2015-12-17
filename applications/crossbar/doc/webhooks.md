/*
Section: Crossbar
Title: Webhooks
Language: en-US
Version: 3.22
*/

Webhooks allow Kazoo to send HTTP requests to a third-party webserver, alerting that server of events occuring within Kazoo. Typically, events would be fired for new calls, when a call is answered, and when a call is finished, though other events will be added in the future.

## Schema

* `name`: Friendly name for the webhook
* `uri`: The HTTP URI of the third-party webserver (required)
* `http_verb`: The HTTP Verb to send the request with (`get` or `post`)
* `hook`: The event(s) in Kazoo that will trigger a request to `uri`
    * `channel_create`: A new channel has begun
    * `channel_answer`: A channel has been answered
    * `channel_destroy`: A channel has finished
    * `all`: All available webhook events
* `retries`: How many times to retry sending the webhook to `uri`
* `custom_data`: JSON object of custom data to be sent along with the event data to the `uri`
* `enabled`: Boolean, is this webhook enabled for operation

## List Installed Webhooks

Webhooks are installed by the system administrator. You can query Crossbar to see which are installed:

    curl -v -X GET \
    -H "Content-Type:application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/webhooks

    {"auth_token": "{AUTH_TOKEN}",
     "data": [
         {
             "description": "Events when calls end",
             "id": "channel_destroy",
             "name": "channel_destroy"
         },
         {
             "description": "Events when new calls start",
             "id": "channel_create",
             "name": "channel_create"
         },
         {
             "description": "Events for when the channel is answered by the endpoint",
             "id": "channel_answer",
             "name": "channel_answer"
        },
        {
            "description": "Receive notifications when objects in Kazoo are changed",
            "id": "object",
            "modifiers": {
                "action": {
                    "description": "A list of object actions to handle",
                    "items": [
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
                        "callflow",
                        "device",
                        "faxbox",
                        "media",
                        "user",
                        "vmbox"
                    ],
                    "type": "array"
                },
                "types": {
                    "description": "A list of object types to handle",
                    "items": {
                        "type": "string"
                    },
                    "type": "array"
                }
            },
            "name": "object"
        }

     ],
     "page_size": 4,
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION}",
     "status": "success"
     }

Some webhooks will also include a `modifiers` object; these are parameters specific to that webhook that can be used to modify the behaviour of the webhook.

## Sample cURL Requests

### List webhooks

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks

Any webhooks with *disable_reason* in the summary has been auto-disabled.

### Create webhook

    curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks \
    -d '{"data":{"name":"New Calls", "uri":"http://my.{SERVER}/calls/new.php", \
    "http_verb":"post", "hook":"channel_create", "retries":3}}'

### Get details of the webhook

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

### Edit webhook

    curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID} \
    -d '{"data":{"name":"New Calls", "uri":"http://my.{SERVER}/calls/new_calls.php", \
    "http_verb":"post", "hook":"channel_create", "retries":3}}'

### Patch webhook

You can also patch an existing webhook:

    curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID} \
    -d '{"data":{"enabled":true}}'

### Delete a webhook

    curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

### List Attempts

Webhooks tracks attempts to send the hook payloads to your URIs. You can get a listing of the more recent attempts to help debug what went wrong.

#### Account Attempts

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/attempts

    {"auth_token": "c89bc20fd8954f6e67614b99e31b4f58",
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


#### Hook Attempts

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}/attempts

### Re-enable auto-disabled hooks in bulk

Webhooks will auto-disable failing hooks (if Kazoo can't reach your server, or you take too long to respond with `200 OK`, for instance). Especially if you're a reseller with webhooks in your client accounts, it can be tedious to have to iterate through all your accounts and re-enable each hook. Fortunately, you can perform this bulk-enable action against an account or an account and its descendants.

#### Enable an account's hooks

    curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks \
    -d '{"data":{"re-enable":true}}'

#### Enable an account's and descendant accounts' hooks

    curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants/webhooks \
    -d '{"data":{"re-enable":true}}'

## Hook Payload

### Base Payload

* hook_event: The type of hook being fired
* call_direction: "inbound" or "outbound", relative to Kazoo
* timestamp: gregorian timestamp of the event
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

### Hook Specific

* channel_create
    * hook_event: channel_create
* channel_answer
    * hook_event: channel_answer
* channel_destroy
    * hook_event: channel_destroy
    * hangup_cause: SIP Hangup Cause (NORMAL_CLEARING, ORIGINATOR_CANCEL, etc)
    * hangup_code: SIP Hangup Code (404, 503, etc)
* doc
    * hook_event: doc
    * action: doc_created, doc_updated, doc_deleted
    * type: user, vmbox, callflow, account, device, faxbox, media

### Hook Specific Custom Data

#### Doc

To restrict the kind of doc or the action or both. You can set the custom data to:

```json
{
   "type": "user",
   "action": "doc_edited"
}
```
