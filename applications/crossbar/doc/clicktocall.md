# Click To Call

## Schema

Click-to-call allows you to create URLs that can be POSTed to with a phone number or SIP URI and create a phone call from the provided contact information to a destination you have pre-determined.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`auth_required` | Determines if this click to call requires valid auth-tokens when invoked | `boolean()` | `true` | `false` |
`blocking` | Whether to block the API response until the call has been originated | `boolean()` | `false` | `false` |
`bypass_media` | Default bypass media mode (The string type is deprecated, please use this as a boolean) | `boolean() | string('true' | 'false' | 'auto')` |   | `false` |
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



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/clicktocall

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
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

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
```
