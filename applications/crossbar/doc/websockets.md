# Websockets

## About Websockets

Fetch information about what bindings can be subscribed to, what sockets are active, and the active bindings of a socket.

## Available Websocket Bindings

Lists all available Websocket bindings.

> GET /v2/websockets

```shell
curl -v -X GET \
    http://{SERVER}:8000/v2/websockets
```

**Response**

```json
{
    "data": {
        "call": [
            "call.CHANNEL_CREATE.{CALL_ID}",
            "call.CHANNEL_ANSWER.{CALL_ID}",
            "call.CHANNEL_DESTROY.{CALL_ID}",
            "call.CHANNEL_HOLD.{CALL_ID}",
            "call.CHANNEL_UNHOLD.{CALL_ID}",
            "call.CHANNEL_BRIDGE.{CALL_ID}",
            "call.PARK_PARKED.{CALL_ID}",
            "call.PARK_RETRIEVED.{CALL_ID}",
            "call.PARK_ABANDONED.{CALL_ID}"
        ],
        "conference": [
            "conference.event.{CONFERENCE_ID}.{CALL_ID}",
            "conference.command.{CONFERENCE_ID}"
        ],
        "fax": [
            "fax.status.{FAX_ID}",
            "fax.object.{ACTION}"
        ],
        "object": [
            "object.doc_created.account",
            "object.doc_created.callflow",
            "object.doc_created.device",
            "object.doc_created.faxbox",
            "object.doc_created.media",
            "object.doc_created.user",
            "object.doc_created.vmbox",
            "object.doc_created.fax",
            "object.doc_created.mailbox_message",
            "object.doc_created.call_recording",
            "object.doc_edited.account",
            "object.doc_edited.callflow",
            "object.doc_edited.device",
            "object.doc_edited.faxbox",
            "object.doc_edited.media",
            "object.doc_edited.user",
            "object.doc_edited.vmbox",
            "object.doc_edited.fax",
            "object.doc_edited.mailbox_message",
            "object.doc_edited.call_recording",
            "object.doc_deleted.account",
            "object.doc_deleted.callflow",
            "object.doc_deleted.device",
            "object.doc_deleted.faxbox",
            "object.doc_deleted.media",
            "object.doc_deleted.user",
            "object.doc_deleted.vmbox",
            "object.doc_deleted.fax",
            "object.doc_deleted.mailbox_message",
            "object.doc_deleted.call_recording"
        ]
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}"
}
```

#### Fetch Socket IDs

> GET /v2/accounts/{ACCOUNT_ID}/websockets

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/websockets
```

```json
{
    "data": [
      {"bindings":["object.doc_created.user"]
       ,"websocket_session_id":"{SOCKET_ID}"
       ,"timestamp":{CONNECTION_TIMESTAMP}
       ,"destination":"{WS_SERVER}"
       ,"source":"{CLIENT_IP}"
      }
    ],
    "status": "success"
}
```


#### Fetch Socket's Bindings

> GET /v2/accounts/{ACCOUNT_ID}/websockets/{SOCKET_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/websockets/{SOCKET_ID}
```

```json
{"data":{
   "bindings": ["{CLIENT_BINDING}"],
   "timestamp":{CONNECTION_TIMESTAMP},
   "destination":"{BLACKHOLE_SERVER}",
   "source":"{CLIENT_IP}",
   "websocket_session_id": "{SOCKET_ID}"
 },
 "status": "success"
}
```
