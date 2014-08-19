/*
Section: Crossbar
Title: Webhooks
Language: en-US
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

## Sample curl Requests

### List webhooks

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/webhooks

### Create webhook

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/webhooks -d '{"data":{"name":"New Calls", "uri":"http://my.server.com/calls/new.php", "http_verb":"post", "hook":"channel_create", "retries":3}}'

### Get details of the webhook

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

### Edit webhook

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID} -d '{"data":{"name":"New Calls", "uri":"http://my.server.com/calls/new_calls.php", "http_verb":"post", "hook":"channel_create", "retries":3}}'

### Delete a webhook

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

## Hook Payload

### Base Payload

* hook\_event: The type of hook being fired
* call_direction: "inbound" or "outbound", relative to Kazoo
* timestamp: gregorian timestamp of the event
* account_id: ID of the account generating the event
* request: SIP Request
* to: SIP To
* from: SIP From
* call_id: SIP Call ID
* other\_leg\_call\_id: If bridged, the Call ID of the other leg
* caller\_id\_name: Caller ID Name
* caller\_id\_number: Caller ID Number
* callee\_id\_name: Callee Name
* callee\_id\_number: Callee Number

Most of these fields should be present on all payloads.

### Hook Specific

* channel\_create
    * hook\_event: channel\_create
* channel\_answer
    * hook\_event: channel\_answer
* channel_destroy
    * hook\_event: channel\_destroy
    * hangup\_cause: SIP Hangup Cause (NORMAL\_CLEARING, ORIGINATOR_CANCEL, etc)
    * hangup_code: SIP Hangup Code (404, 503, etc)
