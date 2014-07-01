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
