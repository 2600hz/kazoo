

#### Comments

* GET - Get bindings.
* GET - Get sockets ID.
* GET - Get socket bindings.

##### Fetch bindings:

###### Request

- Verb: `GET`
- Url: `/v2/websockets`
- Payload: None

###### Response

```json
{
    "data": {
        "call": [
            "call.CHANNEL_CREATE.*",
            "call.CHANNEL_ANSWER.*",
            "call.CHANNEL_DESTROY.*"
            "call.CHANNEL_BRIDGE.*"
        ],
        "fax": [
            "fax.status.*"
        ]
    },
    "status": "success"
}
```

##### Fetch Sockets ID:

###### Request

- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}/websockets`
- Payload: None

###### Response

```json
{
  "data": ["{{ID1}}", "{{ID2}}", "{{ID3}}"],
  "status": "success"
}
```

##### Fetch Socket Bindings:

###### Request

- Verb: `PUT`
- Url: `/v2/accounts/{{ACCOUNT_ID}/websockets/{{ID}}`
- Payload: None

###### Response

```json
{
    "data": [{
        "account_id": "{{ACCOUNT_ID}",
        "auth_token": "{{AUTH_TOKEN}",
        "bindings": [
            "call.CHANNEL_DESTROY.*",
            "call.CHANNEL_ANSWER.*",
            "call.CHANNEL_CREATE.*"
        ],
        "websocket_session_id": "{{ID}}"
    }],
    "status": "success"
}
```
