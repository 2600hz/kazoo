# Webhooks and Websockets



## About Myself

1.  About Myself

    -   James Aimonetti
    -   Kazoo Architect and Community at 2600Hz
    -   mc\_ on freenode

2.  Presenter

    <div class="org-center">
    ![img](./images/presenter_h280.jpg)
    </div>


## Webhooks - High level

-   Request events to be sent to your server
-   Read-only
-   AMQP->HTTP bridge
-   Custom webhooks via callflows


## Webhooks

-   Simple HTTP request is sent from Kazoo to your server
    -   Respond with 200 only (auto-disabled otherwise)
    -   If possible, relay data to separate process for use
-   Update rows in a database or spreadsheet
-   Screen pops
-   Use with Pivot to build reactive routing logic


## Available Webhooks

-   Code can be found [here](https://github.com/2600hz/kazoo/tree/master/applications/webhooks/src/modules)
-   Channel events
    -   CREATE
    -   ANSWER
    -   BRIDGE
    -   DESTROY
-   Inbound/outbound faxes


## Available Webhooks (cont)

-   Parked calls
-   Account objects
    -   Accounts, callflows, devices, fax boxes, media (MOH, IVR, voicemails), users, voicemail boxes, faxes
    -   Created/edited/deleted
    -   Billing systems
-   Custom webhooks from callflows
    -   Analytics on IVRs


## Query for webhooks available

```shell
curl -v -X GET http://{SERVER}:8000/v2/webhooks
```


## Creating a webhook

```shell
curl -v -X PUT \
-H "X-Auth-Token: {AUTH_TOKEN}" \
-H "Content-Type: application/json" \
-d '{"data": {
    "name": "New Calls",
    "uri": "http://{YOUR_SERVER}/calls/new.php",
    "http_verb": "post",
    "hook": "channel_create",
    "retries":3
}}' \
http://{CB_SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks
```


## Webhook Request

-   **GET**: query string parameters
-   **POST**: `x-www-urlencoded-form` body
-   Data sent will be the AMQP JSON payload, encoded for the HTTP method chosen
-   No processing of the response (except HTTP response code)


## The future of webhooks

-   Reseller hooks for all sub-accounts
    -   Added Oct 6
    -   Create one hook, receive events for all sub-accounts
-   Feature requests welcome!


## Websockets

-   Persistent connection
-   Lower overhead (no polling)
-   Get events **and** issue commands to Kazoo
-   Richer UI interactions with Kazoo (operator console)


## Anatomy of websocket connection

<div class="org-center">
![img](./images/WebSockets-Diagram.png "![img](//www.pubnub.com/wp-content/uploads/2014/09/WebSockets-Diagram.png)")
</div>


## Create a websocket connection

-   Ensure **blackhole** application is running
-   Open a connection


## Subscribe for events


## Subscribe for an account's new calls


## Subscribe for multiple types of events


## Subscribe for doc change events


## Unsubscribe from events


## Monster makes this easy

-   See [the docs](https://github.com/2600hz/monster-ui/blob/master/docs/kazoocon.md) for specifics


## Monster makes this easy (cont)


## Query for available bindings

```shell
curl -v -X GET http://{CROSSBAR}:8000/v2/websockets | python -mjson.tool
```

```json
{
    "data": {
        "call": [
            {
                "binding": "call.CHANNEL_CREATE.*",
                "event": "CHANNEL_CREATE"
            },
            {
                "binding": "call.CHANNEL_ANSWER.*",
                "event": "CHANNEL_ANSWER"
            },
            {
                "binding": "call.CHANNEL_DESTROY.*",
                "event": "CHANNEL_DESTROY"
            },
            {
                "binding": "call.CHANNEL_BRIDGE.*",
                "event": "CHANNEL_BRIDGE"
            }
        ]
        ,...
    }
}
```


## Event categories available

-   Channel events
-   Fax events
-   Objects - identical to webhooks


## Fetch account's socket connections

```shell
curl -v -X GET \
-H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/websockets
```

```json
{
    "data": [
        "{SOCKET_ID1}",
        "{SOCKET_ID2}",
        "{SOCKET_ID3}"
    ],
    "status": "success"
}
```


## Fetch bindings for a socket ID

```shell
curl -v -X GET \
-H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/websockets/{SOCKET_ID}
```

```json
{
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "auth_token": "{AUTH_TOKEN}",
            "bindings": [
                "call.CHANNEL_DESTROY.*",
                "call.CHANNEL_ANSWER.*",
                "call.CHANNEL_CREATE.*"
            ],
            "websocket_session_id": "{SOCKET_ID}"
        }
    ],
    "status": "success"
}
```


## Execute commands

```shell
curl -v -X PUT \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "metaflow", "data": { "module", "hangup" }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```


## The future

-   Support sending commands
    -   Currently, `cb_channels` is the way (via metaflows/konami)
    -   Pivot over websockets (great idea from yesterday!)
-   More events exposed for building richer UIs
    -   Conference events
    -   Notifications (vm left, system alerts)


# Thank You