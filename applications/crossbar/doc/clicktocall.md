# Click To Call

#### Schema

Click-to-call allows you to create URLs that can be POSTed to with a phone number or SIP URI and create a phone call from the provided contact information to a destination you have pre-determined.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`auth_required` | Determines if this click to call requires valid auth-tokens when invoked | `boolean()` | `true` | `false` |  
`bypass_media` | Default bypass media mode (The string type is deprecated, please use this as a boolean) | `boolean() | string('auto' | 'false' | 'true')` |   | `false` |  
`caller_id_number` | Explicitly set caller id number | `string()` |   | `false` |  
`custom_application_vars./[a-zA-Z0-9\-_]+/` |   | `string()` |   | `false` |  
`custom_application_vars` | Key-value pairs to set as custom_application_vars on the channel | `object()` | `{}` | `false` |  
`custom_sip_headers.in` | Custom SIP Headers to be applied to calls inbound to Kazoo from the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`custom_sip_headers.out` | Custom SIP Headers to be applied to calls outbound from Kazoo to the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`custom_sip_headers.^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  
`custom_sip_headers` | A property list of SIP headers | `object()` |   | `false` |  
`dial_first` | Determines what will be dialed first: extension or contact | `string('extension' | 'contact')` |   | `false` |  
`extension` | The extension to connect to when the click to call is invoked | `string()` |   | `true` |  
`media.ignore_early_media` | The option to determine if early media from the endpoint should always be ignored | `boolean()` |   | `false` |  
`media` |   | `object()` |   | `false` |  
`music_on_hold.media_id` | The ID of a media object that should be used as the music on hold | `string(0..2048)` |   | `false` |  
`music_on_hold` | The music on hold parameters used if not a property of the device owner | `object()` |   | `false` |  
`name` | A friendly name for the click to call | `string(1..128)` |   | `true` |  
`outbound_callee_id_name` | Callee ID Name of the device calling out to the contact number | `string()` |   | `false` |  
`outbound_callee_id_number` | Callee ID Number of the device calling out to the contact number | `string()` |   | `false` |  
`presence_id` | Static presence ID (used instead of SIP username) | `string()` |   | `false` | `supported`
`ringback` | Ringback to use | `string()` |   | `false` |  
`throttle` | The rate that this click to call can be invoked | `integer()` |   | `false` |  
`timeout` | How long, in seconds, to wait for the call to progress | `integer()` |   | `false` |  
`whitelist.[]` |   | `string(1..)` |   | `false` |  
`whitelist` | A list of regular expressions that the click to call can dial to | `array(string(1..))` |   | `false` |  

### custom_sip_headers

Custom SIP headers applied to an INVITE


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  



## List all clicktocall endpoints

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall
```

```json
{
    "auth_token":"{AUTH_TOKEN}",
    "data": [
        {
            "extension": "{EXTENSION}",
            "id": "{C2C_ID}",
            "name": "{NAME}"
        }
    ],
    "node": "{NODE_HASH}",
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.3.1"
}
```

## Create a clicktocall endpoint

> PUT /v2/accounts/{ACCOUNT_ID}/clicktocall

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"name":"{NAME}, "auth_required":false, "extension":"{EXTENSION}"}}'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall
```

```json
{
    "auth_token":"{AUTH_TOKEN}",
    "data": {
        "auth_required": false,
        "custom_application_vars": {},
        "extension": "{EXTENSION}",
        "id": "{C2C_ID}",
        "name": "{NAME}"
    },
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.3.1"
}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

```json
{
    "auth_token":"{AUTH_TOKEN}",
    "data": {
        "auth_required": false,
        "custom_application_vars": {},
        "extension": "{EXTENSION}",
        "id": "{C2C_ID}",
        "name": "{NAME}"
    },
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.3.1"
}

```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/history

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/history
```

## Execute the clicktocall with a supplied number

> GET/POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect

### Non-blocking version

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect?contact={CONTACT}
```

Will return immediately with a 202

```json
    "data": {
        "application_data": {
            "route": "{CONTACT}"
        },
        "application_name": "transfer",
        "continue_on_fail": true,
        "custom_application_vars": {
            "contact": "{CONTACT}"
        },
        "custom_channel_vars": {
            "account_id": "{ACCOUNT_ID}",
            "authorizing_id": "{C2C_ID}",
            "authorizing_type": "clicktocall",
            "auto_answer_loopback": true,
            "from_uri": "{EXTENSION}@{ACCOUNT_REALM}",
            "inherit_codec": false,
            "loopback_request_uri": "{CONTACT}@{ACCOUNT_REALM}",
            "request_uri": "{CONTACT}@{ACCOUNT_REALM}",
            "retain_cid": true
        },
        "dial_endpoint_method": "single",
        "endpoints": [
            {
                "invite_format": "loopback",
                "route": "{EXTENSION}",
                "to_did": "{EXTENSION}",
                "to_realm": "{ACCOUNT_REALM}"
            }
        ],
        "export_custom_channel_vars": [
            "Account-ID",
            "Authorizing-ID",
            "Authorizing-Type",
            "Loopback-Request-URI",
            "From-URI",
            "Request-URI"
        ],
        "ignore_early_media": true,
        "loopback_bowout": "false",
        "outbound_call_id": "c2c-{C2C_ID}-{RANDOM}",
        "outbound_callee_id_name": "{EXTENSION}",
        "outbound_callee_id_number": "{EXTENSION}",
        "outbound_caller_id_name": "{C2C NAME}",
        "outbound_caller_id_number": "{CONTACT}",
        "simplify_loopback": "false",
        "start_control_process": "false",
        "timeout": 30
    },
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.3.1"
}

```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
```
