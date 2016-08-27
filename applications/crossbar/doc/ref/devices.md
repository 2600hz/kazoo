### Devices

#### About Devices

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
`contact_list` | Contect List Parameters | `object` | `{}` | `false`
`contact_list.exclude` | If set to true the device is excluded from the contact list | `boolean` |   | `false`
`device_type` | Arbitrary device type used by the UI and billing system | `string` |   | `false`
`dial_plan` | A list of rules used to modify dialed numbers | `object` | `{}` | `false`
`do_not_disturb` | DND Parameters | `object` |   | `false`
`do_not_disturb.enabled` | Is do-not-disturb enabled for this device? | `boolean` |   | `false`
`enabled` | Determines if the device is currently enabled | `boolean` | `true` | `false`
`exclude_from_queues` | Do not ring this device when calling user/agent in queue | `boolean` | `false` | `false`
`language` | The language for the device | `string` |   | `false`
`media` | The device media parameters | `object` | `{}` | `false`
`media.audio` | The audio media parameters | `object` | `{}` | `false`
`media.audio.codecs` | A list of audio codecs the device supports | `array(string('OPUS', 'CELT@32000h', 'G7221@32000h', 'G7221@16000h', 'G722', 'speex@32000h', 'speex@16000h', 'PCMU', 'PCMA', 'G729', 'GSM', 'CELT@48000h', 'CELT@64000h', 'G722_16', 'G722_32', 'CELT_48', 'CELT_64', 'Speex', 'speex'))` | `["PCMU"]` | `false`
`media.audio.codecs.[]` |   | `string` |   | `false`
`media.bypass_media` | Default bypass media mode | `boolean, string('true', 'false', 'auto')` |   | `false`
`media.encryption` | Encryption Parameters | `object` | `{}` | `false`
`media.encryption.enforce_security` | Is Encryption Enabled? | `boolean` | `false` | `false`
`media.encryption.methods` | Supported Encryption Types | `array(string('zrtp', 'srtp'))` | `[]` | `false`
`media.encryption.methods.[]` |   | `string` |   | `false`
`media.fax_option` | Is T.38 Supported? | `boolean` |   | `false`
`media.ignore_early_media` | The option to determine if early media from the device should always be ignored | `boolean` |   | `false`
`media.progress_timeout` | The progress timeout to apply to the device (seconds) | `integer` |   | `false`
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
`provision.feature_keys` | Feature Keys | `object` |   | `false`
`provision.feature_keys.^[0-9]+$` |   | `object` |   | `false`
`provision.feature_keys.^[0-9]+$.type` | Feature key type | `string('presence', 'parking', 'personal_parking', 'speed_dial')` |   | `true`
`provision.feature_keys.^[0-9]+$.value` | Feature key value | `string, integer` |   | `true`
`register_overwrite_notify` | When true enables overwrite notifications | `boolean` | `false` | `false`
`ringtones` | Ringtone Parameters | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string(0..256)` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string(0..256)` |   | `false`
`sip` | SIP Parameters | `object` | `{}` | `false`
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


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/devices

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/devices

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/devices/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/status
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{NUMBER}
```

