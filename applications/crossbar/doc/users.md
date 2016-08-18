### Users

#### About Users

Users represent just that, your users of the system. You can assign multiple devices to a user, put the user in a callflow, and all devices will ring.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_forward` | The device call forward parameters | `object` |   | `false`
`call_forward.direct_calls_only` | Determines if the calls that are not directly sent to the device should be forwarded | `boolean` | `false` | `false`
`call_forward.enabled` | Determines if the call forwarding should be used | `boolean` | `false` | `false`
`call_forward.failover` | Enable the call-forwarding parameters if the device is offline | `boolean` | `false` | `false`
`call_forward.ignore_early_media` | The option to determine if early media from the call forwarded number should ignored | `boolean` | `true` | `false`
`call_forward.keep_caller_id` | Determines if the caller id is kept when the call is forwarded, if not the devices caller id is used | `boolean` | `true` | `false`
`call_forward.number` | The number to forward calls to | `string(0..35)` |   | `false`
`call_forward.require_keypress` | Determines if the callee is prompted to press 1 to accept the call | `boolean` | `true` | `false`
`call_forward.substitute` | Determines if the call forwarding replaces the device | `boolean` | `true` | `false`
`call_restriction` | Device level call restrictions for each available number classification | `object` | `{}` | `false`
`call_waiting` |   |   |   | `false`
`caller_id` | The device caller ID parameters | `object` | `{}` | `false`
`contact_list` |   | `object` | `{}` | `false`
`contact_list.exclude` | If set to true the device is excluded from the contact list | `boolean` |   | `false`
`dial_plan` | A list of rules used to modify dialed numbers | `object` | `{}` | `false`
`directories` | Provides the mappings for what directory the user is a part of (the key), and what callflow (the value) to invoke if the user is selected by the caller. | `object` |   | `false`
`do_not_disturb` |   | `object` |   | `false`
`do_not_disturb.enabled` | Is do-not-disturb enabled for this user? | `boolean` |   | `false`
`email` | The email of the user | `string(1..254)` |   | `false`
`enabled` | Determines if the user is currently enabled | `boolean` | `true` | `false`
`first_name` | The first name of the user | `string(1..128)` |   | `true`
`hotdesk` | The user hotdesk parameters | `object` | `{}` | `false`
`hotdesk.enabled` | Determines if the user has hotdesking enabled | `boolean` | `false` | `false`
`hotdesk.id` | The users hotdesk id | `string(0..15)` |   | `false`
`hotdesk.keep_logged_in_elsewhere` | Determines if user should be able to login to mutliple phones simultaneously | `boolean` | `false` | `false`
`hotdesk.pin` | The users hotdesk pin number | `string(4..15)` |   | `false`
`hotdesk.require_pin` | Determines if user requires a pin to change the hotdesk state | `boolean` | `false` | `false`
`language` | The language for this user | `string` |   | `false`
`last_name` | The last name of the user | `string(1..128)` |   | `true`
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
`media.video.codecs` | A list of video codecs the device supports | `array(string('H261', 'H263', 'H264', 'VP8'))` | `[]` | `false`
`media.video.codecs.[]` |   | `string` |   | `false`
`metaflows` | The device metaflow parameters |   |   | `false`
`music_on_hold` | The music on hold parameters used if not a property of the device owner | `object` | `{}` | `false`
`music_on_hold.media_id` | The ID of a media object that should be used as the music on hold | `string(0..128)` |   | `false`
`presence_id` | Static presence ID (used instead of SIP username) | `string` |   | `false`
`priv_level` | The privilege level of the user | `string('user', 'admin')` | `user` | `false`
`profile` | User's profile data | `object` | `{}` | `false`
`pronounced_name` | Name pronounced by user to introduce himself to conference members | `object` |   | `false`
`pronounced_name.media_id` | The ID of a media object that should be used as the music on hold | `string(0..128)` |   | `false`
`require_password_update` | UI flag that the user should update their password. | `boolean` | `false` | `false`
`ringtones` |   | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string(0..256)` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string(0..256)` |   | `false`
`timezone` | User's timezone | `string` |   | `false`
`username` | The GUI login username - alpha-numeric, dashes, at symbol, periods, plusses, and underscores allowed | `string(1..256)` |   | `false`
`verified` | Determines if the user has been verified | `boolean` | `false` | `false`
`vm_to_email_enabled` | Determines if the user would like voicemails emailed to them | `boolean` | `true` | `false`

#### Fetch summary of users in account

> GET /v2/accounts/{ACCOUNT_ID}/users

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "email": "user1@account_realm.com",
            "features": [
                "caller_id",
                "vm_to_email"
            ],
            "first_name": "User",
            "id": "{USER_ID}",
            "last_name": "One",
            "priv_level": "admin",
            "timezone": "America/Los_Angeles",
            "username": "user1@account_realm.com"
        },
        {
            "email": "user2@account_realm.com",
            "features": [
                "caller_id",
                "vm_to_email"
            ],
            "first_name": "User",
            "id": "{USER_ID}",
            "last_name": "Two",
            "priv_level": "user",
            "timezone": "America/Los_Angeles",
            "username": "user2@account_realm.com"
        }
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Create a new user

> PUT /v2/accounts/{ACCOUNT_ID}/users

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    -H "Content-Type: application/json" \
    -d '{"data":{"first_name":"User", "last_name":"Three"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "first_name": "User",
        "hotdesk": {
            "enabled": false,
            "keep_logged_in_elsewhere": false,
            "require_pin": false
        },
        "id": "{USER_ID}",
        "last_name": "Three",
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
        "priv_level": "user",
        "profile": {},
        "require_password_update": false,
        "ringtones": {},
        "verified": false,
        "vm_to_email_enabled": true
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Remove a user

This request will return the current JSON object of the now-deleted user.

> DELETE /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": false,
        "first_name": "User",
        "hotdesk": {
            "enabled": false,
            "keep_logged_in_elsewhere": false,
            "require_pin": false
        },
        "id": "{USER_ID}",
        "last_name": "Three",
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
        "priv_level": "user",
        "profile": {},
        "require_password_update": false,
        "ringtones": {},
        "verified": false,
        "vm_to_email_enabled": true
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch a user

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
{
    "auth_token": "e4c49cfb8b91784c672222d3291b9449",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": true,
        "first_name": "User",
        "hotdesk": {
            "enabled": false,
            "keep_logged_in_elsewhere": false,
            "require_pin": false
        },
        "id": "{USER_ID}",
        "last_name": "Three",
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
        "priv_level": "user",
        "profile": {},
        "require_password_update": false,
        "ringtones": {},
        "verified": false,
        "vm_to_email_enabled": true
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Patch a user's doc

> PATCH /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"enabled":false}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": false,
        "first_name": "User",
        "hotdesk": {
            "enabled": false,
            "keep_logged_in_elsewhere": false,
            "require_pin": false
        },
        "id": "{USER_ID}",
        "last_name": "Three",
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
        "priv_level": "user",
        "profile": {},
        "require_password_update": false,
        "ringtones": {},
        "verified": false,
        "vm_to_email_enabled": true
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Change the user doc

This requires posting the full user's document in the request body

**Sync**: See [the documentation on device sync](#sync) for more info on `check-sync`. One can add the field `"sync": true` to the JSON document in order to attempt a `check-sync` on every registered device this user has.

> POST /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"first_name":"User","last_name":"Three","call_restriction":{},"caller_id":{},"contact_list":{},"dial_plan":{},"enabled":false,"hotdesk":{"enabled":false,"keep_logged_in_elsewhere":false,"require_pin":false},"media":{"audio":{"codecs":["PCMU"]},"encryption":{"enforce_security":false,"methods":[]},"video":{"codecs":[]}},"music_on_hold":{},"priv_level":"user","profile":{},"require_password_update":false,"ringtones":{},"verified":false,"vm_to_email_enabled":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "call_restriction": {},
        "caller_id": {},
        "contact_list": {},
        "dial_plan": {},
        "enabled": false,
        "first_name": "User",
        "hotdesk": {
            "enabled": false,
            "keep_logged_in_elsewhere": false,
            "require_pin": false
        },
        "id": "{USER_ID}",
        "last_name": "Three",
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
        "priv_level": "user",
        "profile": {},
        "require_password_update": false,
        "ringtones": {},
        "verified": false,
        "vm_to_email_enabled": true
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch (or create) a vCard

[vCard](https://en.wikipedia.org/wiki/VCard) is a file format typically used in emails as a form of business card. Kazoo currently generates a 3.0 compatible vCard.

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/vcard

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    -H "Accept: text/x-vcard"
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/vcard
BEGIN:VCARD
VERSION:3.0
FN:User Three
N:Three;User
END:VCARD
```

#### Remove the photo from the user

> DELETE /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/photo

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/photo
```

#### Fetch the user's photo, if any

Set the `Accept` header to either `application/base64` or `application/octet-stream` to retrieve the picture's contents.

If the result is successful, you will want to pipe the response into a file.

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/photo

```shell
curl -v -X GET \
    -H "Accept: application/base64" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/photo
[binary data]
```

#### Create or change the user's photo

Use `application/octet-stream` as the content type.

> POST /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/photo

```shell
curl -v -X POST \
    -H "Content-Type: application/octet-stream" \
    --data-binary @/path/to/image.jpg \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/photo
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Execute a quick call

Ring user's devices; once answered, connect to `{NUMBER}`

In this scenario, the user's devices are considered the `callee` while the `{NUMBER}` side is considered the caller (helpful to know when debugging a call!).

Query string options:

Key | Type | Description
--- | ---- | -----------
`auto_answer` | `boolean()` | Tells the SIP phone to auto-answer the call, if supported
`cid-name` | `string()` | Set the caller ID name (defaults to "Device QuickCall")
`cid-number` | `string()` | Set the caller ID number (defaults to the `{NUMBER}`)
`ignore-early-media` | `boolean()` | Toggle whether to ignore [early media](https://freeswitch.org/confluence/display/FREESWITCH/Early+Media)
`media` | `string('bypass', 'process')` | Toggle whether to go peer-to-peer([bypass](https://freeswitch.org/confluence/display/FREESWITCH/Bypass+Media+Overview) with the RTP
`number_filter` | `boolean()`, `regex()` | If true, remove non-alphanumeric characters. If a regex, use the first capture group as the "number" to dial.
`timeout` | `integer(3..)` | In seconds, how long to ring the device(s) (defaults to 30)

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/quickcall/{NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/quickcall/{NUMBER}
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
      "route": "{NUMBER}"
    },
    "application_name": "transfer"
  },
  "status": "success",
  "request_id": "{REQUEST_ID}",
  "revision": "{REVISION}"
}
```
