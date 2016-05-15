
### Channels

#### About Channels

The Channels API allows queries to find active channels for an account, a user, or a device. Given a call-id for a channel, a limited set of commands are allowed to be executed against that channel (such as hangup, transfer, or play media).

#### Fetch active channels

> GET /v2/accounts/{ACCOUNT_ID}/channels

```curl
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels
{
    "auth_token": "b72c847cbb652606e1d68ed399aff89e",
    "data": [
        {
            "answered": true,
            "authorizing_id": "63fbb9ac78e11f3ccb387928a423798a",
            "authorizing_type": "device",
            "destination": "user_zu0bf7",
            "direction": "outbound",
            "other_leg": "d220c187-e18edc42-bab2459d@10.26.0.91",
            "owner_id": "72855158432d790dfb22d03ff64c033e",
            "presence_id": "user_zu0bf7@account.realm.com",
            "timestamp": 63573977746,
            "username": "user_zu0bf7",
            "uuid": "dab25c76-7479-4ed2-ba92-6b725d68e351"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch channels for a user or device

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels

```curl
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels
```

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels

```curl
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels
```

#### Fetch a channel's details

> GET /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```curl
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

#### Execute an application against a Channel

##### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | What to execute on the channel | `string('transfer', 'hangup', 'callflow')` | | `true`
`action.transfer` | Transfers the `{UUID}` leg to the `target` extension/DID and places the other leg on hold | | |
`target` | The extension/DID to transfer the `{UUID}` to | string() | |
`takeback_dtmf` | DTMF to cancel the transfer | `string("0".."9","*","#")` | |
`moh` | `media_id` for Music on Hold while transferring | `string()` | |
`ringback` | ringback to play to the transferor | `string()` | |
`action.callflow` | Executes a callflow ID on the `{UUID}` | | |
`id` | Callflow ID to execute | `string()` | |
`action.hangup` | Hangup the `{UUID}` | | |
`action.hold` | Put the Caller on Hold | | |
`moh_aleg` | Music to play on User's Own Call leg | `string()` | |
`moh_bleg` | Music to play on Caller's Call leg | `string()` | |
`unhold_key` | Key to Unhold the Call | `string("0".."9")` | |
`action.move` | Move the Call to a user's `{UUID}` | | |
`data.user` | A user's `{UUID}` | `string()` | |




> POST /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```curl
curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "transfer", "target": "2600", "takeback_dtmf": "*1", "moh": "media_id" }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```
