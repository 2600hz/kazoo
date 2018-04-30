# Quickcall

## About Quickcall

Quickcall allows you to create legs between endpoints (user, device, ...).

### Target Call ID

If you know ahead of time that this new quickcall leg will be interacting with an existing call leg, you can supply the existing call-id to the API call to ensure the new leg will be created on the same media server as the existing call leg.

### Endpoints supported

Supported endpoints for quickcalls:

 Endpoint Type | Endpoint Id
 ------------- | -----------
 `users`       | `{USER_ID}`
 `devices`     | `{DEVICE_ID}`

### Custom Application Vars

CAVs allow you to set custom data that will appear on subsequent call events (found in [Webhook](./webhooks.md) and [Websocket](./websockets.md) payloads) as well as the final CDR.

You can specify CAVs in two way:

* As query-string parameters: `/quickcall/{NUMBER}?foo=bar`
* As POST body: `{"data":{"custom_application_vars":{"foo":"bar"}}}`

## Schema

Request options (query string in a GET or POST body):

Key | Type | Description
--- | ---- | -----------
`auto_answer` | `boolean()` | Tells the SIP phone to auto-answer the call, if supported
`cid-name` | `string()` | Set the caller ID name (defaults to "Device QuickCall")
`cid-number` | `string()` | Set the caller ID number (defaults to the `{PHONE_NUMBER}`)
`custom_application_vars` | `object()` | Custom data to include on the call (and events related to the call)
`ignore-early-media` | `boolean()` | Toggle whether to ignore [early media](https://freeswitch.org/confluence/display/FREESWITCH/Early+Media)
`media` | `string('bypass', 'process')` | Toggle whether to go peer-to-peer([bypass](https://freeswitch.org/confluence/display/FREESWITCH/Bypass+Media+Overview) with the RTP
`number_filter` | `boolean()`, `regex()` | If true, remove non-alphanumeric characters. If a regex, use the first capture group as the "number" to dial.
`target_call_id` | `string()` | An existing call-id used to determine what media server to create the quickcall leg on
`timeout` | `integer(3..)` | In seconds, how long to ring the device(s) (defaults to 30)

## Non-blocking Quickcall

This returns a 202 immediately. The drawback is that if no endpoints are found to originate to, there will be no channel events generated to let an external application know the quickcall failed.

> GET /v2/accounts/{ACCOUNT_ID}/{ENDPOINTS}/{ENDPOINT_ID}/quickcall/{NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/{ENDPOINTS}/{ENDPOINT_ID}/quickcall/{NUMBER}
```

## Blocking Quickcall

This will return a 202 if the quickcall successfully originates (meaning a channel is started). It will return errors if the originate fails to start a channel or if there aren't any endpoints available.

> POST /v2/accounts/{ACCOUNT_ID}/{ENDPOINTS}/{ENDPOINT_ID}/quickcall/{NUMBER}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/{ENDPOINTS}/{ENDPOINT_ID}/quickcall/{NUMBER}
```

## Execute a quick call for a device

Ring the device; once answered, connect to `{PHONE_NUMBER}`

In this scenario, the device is considered the `callee` while the `{PHONE_NUMBER}` side is considered the caller (helpful to know when debugging a call!).

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{PHONE_NUMBER}
```

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "export_custom_channel_vars": [
      "Account-ID",
      "Retain-CID",
      "Authorizing-ID",
      "Authorizing-Type"
    ],
    "custom_channel_vars": {
      "authorizing_id": "{DEVICE_ID}",
      "authorizing_type": "device",
      "inherit_codec": "false",
      "retain_cid": "true",
      "account_id": "{ACCOUNT_ID}"
    },
    "continue_on_fail": false,
    "dial_endpoint_method": "simultaneous",
    "outbound_callee_id_number": "{DEVICE_CALLER_ID_NUMBER}",
    "outbound_callee_id_name": "{DEVICE_CALLER_ID_NAME}",
    "outbound_caller_id_number": "{E164_NUMBER}",
    "outbound_caller_id_name": "Device QuickCall",
    "media": "process",
    "ignore_early_media": true,
    "timeout": 30,
    "endpoints": [
      {
        "outbound_call_id": "{CALL_ID}-quickcall",
        "custom_channel_vars": {
          "auto_answer": true,
          "authorizing_id": "{DEVICE_ID}",
          "owner_id": "{USER_ID}",
          "account_id": "{ACCOUNT_ID}",
          "media_encryption_enforce_security": false,
          "sip_invite_domain": "{ACCOUNT_REALM}"
        },
        "custom_sip_headers": {
          "x_kazoo_aor": "sip:{DEVICE_SIP_USER}@{ACCOUNT_REALM}"
        },
        "presence_id": "{PRESENCE_ID}",
        "codecs": [
          "PCMU",
          "PCMA"
        ],
        "endpoint_id": "{DEVICE_ID}",
        "to_did": "{E164_NUMBER}",
        "to_realm": "{ACCOUNT_REALM}",
        "to_username": "{DEVICE_SIP_USER}",
        "to_user": "{DEVICE_SIP_USER}",
        "invite_format": "username"
      }
    ],
    "application_data": {
      "route": "{PHONE_NUMBER}"
    },
    "application_name": "transfer"
  },
  "status": "success",
  "request_id": "{REQUEST_ID}",
  "revision": "{REVISION}"
}
```

## Execute a quick call for a user

Ring user's devices; once answered, connect to `{PHONE_NUMBER}`

In this scenario, the user's devices are considered the `callee` while the `{PHONE_NUMBER}` side is considered the caller (helpful to know when debugging a call!).

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/quickcall/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/quickcall/{PHONE_NUMBER}
```

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "export_custom_channel_vars": [
      "Account-ID",
      "Retain-CID",
      "Authorizing-ID",
      "Authorizing-Type"
    ],
    "custom_channel_vars": {
      "authorizing_id": "{USER_ID}",
      "authorizing_type": "user",
      "inherit_codec": "false",
      "retain_cid": "true",
      "account_id": "{ACCOUNT_ID}"
    },
    "continue_on_fail": false,
    "dial_endpoint_method": "simultaneous",
    "outbound_callee_id_number": "{DEVICE_CALLER_ID_NUMBER}",
    "outbound_callee_id_name": "{DEVICE_CALLER_ID_NAME}",
    "outbound_caller_id_number": "{E164_NUMBER}",
    "outbound_caller_id_name": "Device QuickCall",
    "media": "process",
    "ignore_early_media": true,
    "timeout": 30,
    "endpoints": [
      {
        "outbound_call_id": "{CALL_ID}-quickcall",
        "custom_channel_vars": {
          "auto_answer": true,
          "authorizing_id": "{USER_ID}",
          "owner_id": "{USER_ID}",
          "account_id": "{ACCOUNT_ID}",
          "media_encryption_enforce_security": false,
          "sip_invite_domain": "{ACCOUNT_REALM}"
        },
        "custom_sip_headers": {
          "x_kazoo_aor": "sip:{DEVICE_SIP_USER}@{ACCOUNT_REALM}"
        },
        "presence_id": "{PRESENCE_ID}",
        "codecs": [
          "PCMU",
          "PCMA"
        ],
        "endpoint_id": "{DEVICE_ID}",
        "to_did": "{E164_NUMBER}",
        "to_realm": "{ACCOUNT_REALM}",
        "to_username": "{DEVICE_SIP_USER}",
        "to_user": "{DEVICE_SIP_USER}",
        "invite_format": "username"
      }
    ],
    "application_data": {
      "route": "{PHONE_NUMBER}"
    },
    "application_name": "transfer"
  },
  "status": "success",
  "request_id": "{REQUEST_ID}",
  "revision": "{REVISION}"
}
```

## Failing Requests

If issued with a `GET` to an unregistered device (or a user with no available devices), quickcall will return the call setup information immediately but no channel events will be generated (no events for webhooks/websockets). This can lead external apps to not know if the quickcall was originated properly or not.

Therefore, it is advisable to use `POST` which will block the API request until the channel starts or the quickcall fails. An example of a failing quickcall (which generated no call events):

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "error_message": "DESTINATION_OUT_OF_ORDER",
        "request": {
            "app_name": "crossbar",
            "app_version": "4.0.0",
            "application_data": {
                "route": "{EXTENSION}"
            },
            "application_name": "transfer",
            "continue_on_fail": false,
            "custom_channel_vars": {
                "account_id": "{ACCOUNT_ID}",
                "authorizing_id": "{DEVICE_ID}",
                "authorizing_type": "device",
                "inherit_codec": "false",
                "retain_cid": "true"
            },
            "dial_endpoint_method": "simultaneous",
            "endpoints": [
                {
                    "codecs": [
                        "PCMU",
                        "PCMA"
                    ],
                    "custom_channel_vars": {
                        "account_id": "{ACCOUNT_ID}",
                        "authorizing_id": "{DEVICE_ID}",
                        "authorizing_type": "device",
                        "auto_answer": true,
                        "media_encryption_enforce_security": false,
                        "sip_invite_domain": "{SIP_REALM}"
                    },
                    "custom_sip_headers": {
                        "x_kazoo_aor": "sip:{SIP_USERNAME}@{SIP_REALM}",
                        "x_kazoo_invite_format": "contact"
                    },
                    "ignore_completed_elsewhere": false,
                    "invite_format": "contact",
                    "outbound_call_id": "{CALL_ID}-quickcall",
                    "presence_id": "{SIP_USERNAME}@{SIP_REALM}",
                    "to_did": "{EXTENSION}",
                    "to_realm": "{SIP_REALM}",
                    "to_user": "{SIP_USERNAME}",
                    "to_username": "{SIP_USERNAME}"
                }
            ],
            "event_category": "resource",
            "event_name": "originate_req",
            "export_custom_channel_vars": [
                "Account-ID",
                "Retain-CID",
                "Authorizing-ID",
                "Authorizing-Type",
                "Outbound-Callee-ID-Number",
                "Outbound-Callee-ID-Name"
            ],
            "ignore_early_media": true,
            "media": "process",
            "msg_id": "{MSG_ID}",
            "node": "{NODE}",
            "outbound_callee_id_name": "{DEVICE_CALLER_ID_NAME}",
            "outbound_callee_id_number": "{DEVICE_CALLER_ID_NUMBER}",
            "outbound_caller_id_name": "Device QuickCall",
            "outbound_caller_id_number": "{EXTENSION}",
            "server_id": "{API_AMQP_QUEUE}",
            "system_log_id": "{LOG_ID}",
            "timeout": 30
        }
    },
    "error": "500",
    "message": "quickcall initiation failed",
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "status": "error",
    "timestamp": "{TIMESTAMP}",
    "version": "4.2.2"
}
```
