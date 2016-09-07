### Devices

#### About Devices

Devices are the endpoints assigned to an account that serve that account's needs. Devices like fax machines, SIP phones, soft phone clients, and cell phones (via call fowarding), among others, can be represented by Kazoo devices.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_forward` | The device call forward parameters | `object` |   | `false`
`call_forward.direct_calls_only` | Determines if the calls that are not directly sent to the device should be forwarded | `boolean` | `false` | `false`
`call_forward.enabled` | Determines if the call forwarding should be used | `boolean` | `false` | `false`
`call_forward.failover` | Enable the call-forwarding parameters if the device is offline | `boolean` | `false` | `false`
`call_forward.ignore_early_media` | The option to determine if early media from the call forwarded number should ignored | `boolean` | `true` | `false`
`call_forward.keep_caller_id` | Determines if the caller id is kept when the call is forwarded, if not the devices caller id is used | `boolean` | `true` | `false`
`call_forward.number` | The number to forward calls to | `string(0..15)` |   | `false`
`call_forward.require_keypress` | Determines if the callee is prompted to press 1 to accept the call | `boolean` | `true` | `false`
`call_forward.substitute` | Determines if the call forwarding replaces the device | `boolean` | `true` | `false`
`call_restriction` | Device level call restrictions for each available number classification | `object` | `{}` | `false`
`call_waiting` |   | `#/definitions/call_waiting` |   | `false`
`caller_id` | The device caller ID parameters | `object` | `{}` | `false`
`contact_list` |   | `object` | `{}` | `false`
`contact_list.exclude` | If set to true the device is excluded from the contact list | `boolean` |   | `false`
`device_type` | Arbitrary device type used by the UI and billing system | `string` |   | `false`
`dial_plan` | A list of rules used to modify dialed numbers | `object` | `{}` | `false`
`do_not_disturb` |   | `object` |   | `false`
`do_not_disturb.enabled` | Is do-not-disturb enabled for this device? | `boolean` |   | `false`
`enabled` | Determines if the device is currently enabled | `boolean` | `true` | `false`
`exclude_from_queues` | Do not ring this device when calling user/agent in queue | `boolean` | `false` | `false`
`language` | The language for the device | `string` |   | `false`
`media` | The device media parameters | `object` | `{}` | `false`
`media.audio` | The audio media parameters | `object` | `{}` | `false`
`media.audio.codecs` | A list of audio codecs the device supports | `array(string('OPUS', 'CELT@32000h', 'G7221@32000h', 'G7221@16000h', 'G722', 'speex@32000h', 'speex@16000h', 'PCMU', 'PCMA', 'G729', 'GSM', 'CELT@48000h', 'CELT@64000h', 'G722_16', 'G722_32', 'CELT_48', 'CELT_64', 'Speex', 'speex'))` | `PCMU` | `false`
`media.audio.codecs.[]` |   | `string` |   | `false`
`media.bypass_media` | Default bypass media mode | `boolean, string('true', 'false', 'auto')` |   | `false`
`media.encryption` |   | `object` | `{}` | `false`
`media.encryption.enforce_security` |   | `boolean` | `false` | `false`
`media.encryption.methods` |   | `array(string('zrtp', 'srtp'))` | `[]` | `false`
`media.encryption.methods.[]` |   | `string` |   | `false`
`media.fax_option` | Support T.38 | `boolean` |   | `false`
`media.ignore_early_media` | The option to determine if early media from the device should always be ignored | `boolean` |   | `false`
`media.progress_timeout` | The progress timeout to apply to the device | `integer` |   | `false`
`media.video` | The video media parameters | `object` | `{}` | `false`
`media.video.codecs` | A list of video codecs the device supports | `array(string('VP8', 'H264', 'H263', 'H261'))` | `[]` | `false`
`media.video.codecs.[]` |   | `string` |   | `false`
`metaflows` | The device metaflow parameters | `#/definitions/metaflows` |   | `false`
`music_on_hold` | The music on hold parameters used if not a property of the device owner | `object` | `{}` | `false`
`music_on_hold.media_id` | The ID of a media object that should be used as the music on hold | `string(0..128)` |   | `false`
`mwi_unsolicitated_updates` | When true enables unsolicitated mwi notifications | `boolean` | `true` | `false`
`name` | A friendly name for the device | `string(1..128)` |   | `true`
`outbound_flags` | List of flags (features) this device requires when making outbound calls | `array(string)` |   | `false`
`outbound_flags.[]` |   | `string` |   | `false`
`owner_id` | The ID of the user object that 'owns' the device | `string(32)` |   | `false`
`presence_id` | Static presence ID (used instead of SIP username) | `string` |   | `false`
`provision` | Provision data | `object` |   | `false`
`provision.feature_keys` |   | `object` |   | `false`
`provision.feature_keys.^[0-9]+$` |   | `object` |   | `false`
`provision.feature_keys.^[0-9]+$.type` | Feature key type | `string('presence', 'parking', 'personal_parking', 'speed_dial')` |   | `true`
`provision.feature_keys.^[0-9]+$.value` | Feature key value | `string, integer` |   | `true`
`register_overwrite_notify` | When true enables overwrite notifications | `boolean` | `false` | `false`
`ringtones` |   | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string(0..256)` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string(0..256)` |   | `false`
`sip` |   | `object` | `{}` | `false`
`sip.custom_sip_headers` | A property list of SIP headers beging with the prefix 'X-' | `object` |   | `false`
`sip.expire_seconds` | The time, in seconds, sent to the provisioner for the registration period that the device should be configured with. | `integer` | `300` | `false`
`sip.ignore_completed_elsewhere` | When set to false the phone should not consider ring group calls answered elsewhere as missed | `boolean` |   | `false`
`sip.invite_format` | The SIP request URI invite format | `string('username', 'npan', '1npan', 'e164', 'route')` | `username` | `false`
`sip.ip` | IP address for this device | `string` |   | `false`
`sip.method` | Method of authentication | `string('password', 'ip')` | `password` | `false`
`sip.number` | The number used if the invite format is 1npan, npan, or e164 (if not set the dialed number is used) | `string` |   | `false`
`sip.password` | SIP authentication password | `string(5..32)` |   | `false`
`sip.realm` | The realm this device should use, overriding the account realm. Should rarely be necessary. | `string` |   | `false`
`sip.route` | The SIP URL used if the invite format is 'route' | `string` |   | `false`
`sip.static_route` | Sends all inbound calls to this string (instead of dialed number or username) | `string` |   | `false`
`sip.username` | SIP authentication username | `string(2..32)` |   | `false`
`suppress_unregister_notifications` | When true disables deregister notifications | `boolean` | `false` | `false`
`timezone` | Device's timezone | `string` |   | `false`

#### Fetch summary of devices in account

> GET /v2/accounts/{ACCOUNT_ID}/devices

```shell
curl -v -X GET \
    -X "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "device_type": "sip_device",
            "enabled": false,
            "id": "{DEVICE_ID}",
            "mac_address": "00:04:f2:ab:7e:fd",
            "name": "MyPolycom"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Create a new device

See the schema for available fields to include in the data portion

> PUT /v2/accounts/{ACCOUNT_ID}/devices

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"New Device"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "exclude_from_queues": false,
        "id": "{DEVICE_ID}",
        "media": {
            "audio": {
                "codecs": [
                    "PCMU"
                ]
            },
            "encryption": {
                "enforce_security": false,
                "methods": []
            },
            "video": {
                "codecs": []
            }
        },
        "music_on_hold": {},
        "mwi_unsolicitated_updates": true,
        "name": "New Device",
        "register_overwrite_notify": false,
        "ringtones": {},
        "sip": {
            "invite_format": "username",
            "method": "password",
            "registration_expiration": 300
        },
        "suppress_unregister_notifications": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Remove a device

> DELETE /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "exclude_from_queues": false,
        "id": "{DEVICE_ID}",
        "media": {
            "audio": {
                "codecs": [
                    "PCMU"
                ]
            },
            "encryption": {
                "enforce_security": false,
                "methods": []
            },
            "video": {
                "codecs": []
            }
        },
        "music_on_hold": {},
        "mwi_unsolicitated_updates": true,
        "name": "New Device",
        "register_overwrite_notify": false,
        "ringtones": {},
        "sip": {
            "invite_format": "username",
            "method": "password",
            "registration_expiration": 300
        },
        "suppress_unregister_notifications": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}

```

#### Fetch a device

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "exclude_from_queues": false,
        "id": "{DEVICE_ID}",
        "media": {
            "audio": {
                "codecs": [
                    "PCMU"
                ]
            },
            "encryption": {
                "enforce_security": false,
                "methods": []
            },
            "video": {
                "codecs": []
            }
        },
        "music_on_hold": {},
        "mwi_unsolicitated_updates": true,
        "name": "New Device",
        "register_overwrite_notify": false,
        "ringtones": {},
        "sip": {
            "invite_format": "username",
            "method": "password",
            "registration_expiration": 300
        },
        "suppress_unregister_notifications": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Change a device doc

Including `"sync":true` in the "data" will attempt to reboot the phone. See the sync section below.

> POST /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"new device","call_restriction":{},"caller_id":{},"contact_list":{},"dial_plan":{},"enabled":true,"exclude_from_queues":false,"media":{"audio":{"codecs":["PCMU"]},"encryption":{"enforce_security":false,"methods":[]},"video":{"codecs":[]}},"music_on_hold":{},"mwi_unsolicitated_updates":true,"register_overwrite_notify":false,"ringtones":{},"sip":{"invite_format":"username","method":"password","registration_expiration":300},"suppress_unregister_notifications":false,"id":"4f3330e78e664bb57f8fb23fbaac2429"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "exclude_from_queues": false,
        "id": "{DEVICE_ID}",
        "media": {
            "audio": {
                "codecs": [
                    "PCMU"
                ]
            },
            "encryption": {
                "enforce_security": false,
                "methods": []
            },
            "video": {
                "codecs": []
            }
        },
        "music_on_hold": {},
        "mwi_unsolicitated_updates": true,
        "name": "new device",
        "register_overwrite_notify": false,
        "ringtones": {},
        "sip": {
            "invite_format": "username",
            "method": "password",
            "registration_expiration": 300
        },
        "suppress_unregister_notifications": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Patch a device

> PATCH /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"presence_id":"dis_my_device"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "exclude_from_queues": false,
        "id": "{DEVICE_ID}",
        "media": {
            "audio": {
                "codecs": [
                    "PCMU"
                ]
            },
            "encryption": {
                "enforce_security": false,
                "methods": []
            },
            "video": {
                "codecs": []
            }
        },
        "music_on_hold": {},
        "mwi_unsolicitated_updates": true,
        "name": "new device",
        "presence_id":"dis_my_device",
        "register_overwrite_notify": false,
        "ringtones": {},
        "sip": {
            "invite_format": "username",
            "method": "password",
            "registration_expiration": 300
        },
        "suppress_unregister_notifications": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}

```


#### Fetch registration statuses of all devices

This will fetch the current registrations of any devices. If no devices are registered, an empty list will be returned.

> GET /v2/accounts/{ACCOUNT_ID}/devices/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/status
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "device_id": "{DEVICE_ID}",
            "registered": true
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "undefined",
    "status": "success"
}
```

#### Reboot a device

Some devices support receiving SIP NOTIFY packets with `event` = `check-sync`. This is typically used to reboot the phone if the configuration has changed. Kazoo will generate the NOTIFY packet if the device is registered.

> POST /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync
{
    "auth_token": "{AUTH_TOKEN}",
    "data": "sync request sent",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Execute a quick call

Ring the device; once answered, connect to `{PHONE_NUMBER}`

In this scenario, the device is considered the `callee` while the `{PHONE_NUMBER}` side is considered the caller (helpful to know when debugging a call!).

Query string options:

Key | Type | Description
--- | ---- | -----------
`auto_answer` | `boolean()` | Tells the SIP phone to auto-answer the call, if supported
`cid-name` | `string()` | Set the caller ID name (defaults to "Device QuickCall")
`cid-number` | `string()` | Set the caller ID number (defaults to the `{PHONE_NUMBER}`)
`ignore-early-media` | `boolean()` | Toggle whether to ignore [early media](https://freeswitch.org/confluence/display/FREESWITCH/Early+Media)
`media` | `string('bypass', 'process')` | Toggle whether to go peer-to-peer([bypass](https://freeswitch.org/confluence/display/FREESWITCH/Bypass+Media+Overview) with the RTP
`number_filter` | `boolean()`, `regex()` | If true, remove non-alphanumeric characters. If a regex, use the first capture group as the "number" to dial.
`timeout` | `integer(3..)` | In seconds, how long to ring the device(s) (defaults to 30)

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{PHONE_NUMBER}
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

#### Adding Ringtones

You can setup internal and external ringtones by adding this:

```json
{
    "name": "Device with custom ringtones",
    "ringtones": {
        "internal": "alert info SIP header",
        "external": "alert info SIP header"
    }
}
```

See, for instance, the [Polycom example](https://freeswitch.org/confluence/display/FREESWITCH/Polycom+Internal+Ring)

#### Load a user's devices

Often you'll want to see what devices belong to a user, or devices that a user has hot-desked into.

Notice that the first device, `{DEVICE_ID_1}` is owned by `{USER_ID}` but the second device, `{DEVICE_ID_2}`, is owned by `{OWNER_ID}` **and** is currently hotdesked to `{USER_ID}` (see the `"hotdesked":true` attribute).

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/devices

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/devices
    {"auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "device_type": "sip_device",
            "enabled": true,
            "hotdesked": false,
            "id": "{DEVICE_ID_1}",
            "mac_address": "",
            "name": "USER_ID_DEVICE",
            "owner_id": "{USER_ID}"
        },
        {
            "device_type": "sip_device",
            "enabled": true,
            "hotdesked": true,
            "id": "{DEVICE_ID_2}",
            "mac_address": "",
            "name": "OWNER_ID_DEVICE",
            "owner_id": "{OWNER_ID}"
        }
      ],
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION}",
     "status": "success"
    }
```

#### Create an Authn-By-IP Device

Here is a minimal API request that creates a device that will authenticate by IP address instead of username/password

> PUT /v2/accounts/{ACCOUNT_ID}/devices

```shell
    curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"enabled":true,"name":"authn_by_ip","sip":{"invite_format":"e164", "ip":"{IP_ADDRESS}","method":"ip"}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "exclude_from_queues": false,
        "id": "{DEVICE_ID}",
        "media": {
            "audio": {
                "codecs": [
                    "PCMU"
                ]
            },
            "encryption": {
                "enforce_security": false,
                "methods": []
            },
            "video": {
                "codecs": []
            }
        },
        "music_on_hold": {},
        "mwi_unsolicitated_updates": true,
        "name": "authn_by_ip",
        "register_overwrite_notify": false,
        "ringtones": {},
        "sip": {
            "invite_format": "e164",
            "ip": "{IP_ADDRESS}",
            "method": "ip",
            "registration_expiration": 300
        },
        "suppress_unregister_notifications": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
