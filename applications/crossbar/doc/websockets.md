### Websockets

#### About Websockets

#### Schema



#### Fetch

> GET /v2/websockets

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/websockets
```

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
        "{SOCKET_ID1}",
        "{SOCKET_ID2}",
        "{SOCKET_ID3}"
    ],
    "status": "success"
}
```


#### Fetch Socket Bindings

> GET /v2/accounts/{ACCOUNT_ID}/websockets/{SOCKET_ID}

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
