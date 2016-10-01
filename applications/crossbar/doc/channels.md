
### Channels

#### About Channels

The Channels API allows queries to find active channels for an account, a user, or a device. Given a call-id for a channel, a limited set of commands are allowed to be executed against that channel (such as hangup, transfer, or play media).

#### Fetch active channels system wide.

For superduper admin only.
Be sure to set `system_config`->`crossbar.channels`->`system_wide_channels_list` flag to `true`.

> GET /v2/channels

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/channels
```

#### Fetch active channels for an account

> GET /v2/accounts/{ACCOUNT_ID}/channels

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
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

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels
```

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels
```

#### Fetch a channel's details

> GET /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

#### Execute an application against a Channel

##### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | What to execute on the channel | `string('transfer', 'hangup', 'callflow', 'intercept')` | | `true`
`action.transfer` | Transfers the `{UUID}` leg to the `target` extension/DID and places the other leg on hold | | |
`target` | The extension/DID to transfer the `{UUID}` to | string() | |
`takeback_dtmf` | DTMF to cancel the transfer | `string("0".."9","*","#")` | |
`moh` | `media_id` for Music on Hold while transferring | `string()` | |
`ringback` | ringback to play to the transferor | `string()` | |
`action.callflow` | Executes a callflow ID on the `{UUID}` | | |
`id` | Callflow ID to execute | `string()` | |
`action.hangup` | Hangup the `{UUID}` | | |
`action.intercept` | Intercept `{UUID}` to `target` and hangup the other leg| | |
`target_type` | Type of `target` | `string('device', 'user')` | | `true`
`target_id` | Id of device to which current channel will be bridged | `string()` | | `true`
`unbridged_only` | Intercept only unbridged channel | `boolean()` | `true` | 

> POST /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```shell
curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "transfer", "target": "2600", "takeback_dtmf": "*1", "moh": "media_id" }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

#### Put a feature on a channel

currently only `metaflow` is supported

#### Metaflow

Metaflow feature is a `metaflow` object which validates with its json schema.

. reasoning
The `POST` action required that every metaflow action would have to be coded into the module.

. benefits
The metaflow feature allows adding new types of metaflows without changing the code.
It also allows full metaflows and not only single actions, ie, the `children` node is also processed.

> PUT /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```shell
curl -v -X PUT \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "metaflow", "data": { "module", "hangup" }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```
