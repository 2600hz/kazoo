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
`call_waiting` |   | [#/definitions/call_waiting](#call_waiting) |   | `false`
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
`metaflows` | The device metaflow parameters | [#/definitions/metaflows](#metaflows) |   | `false`
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
`provision.feature_keys./^[0-9]+$/` |   | `object` |   | `false`
`provision.feature_keys./^[0-9]+$/.type` | Feature key type | `string('presence', 'parking', 'personal_parking', 'speed_dial')` |   | `true`
`provision.feature_keys./^[0-9]+$/.value` | Feature key value | `string, integer` |   | `true`
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


##### call_waiting

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | Determines if server side call waiting is enabled/disabled | `boolean` |   | `false`

##### caller_id

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`emergency` | The caller ID used when a resource is flagged as 'emergency' | `object` |   | `false`
`emergency.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`emergency.number` | The caller id name for the object type | `string(0..35)` |   | `false`
`external` | The default caller ID used when dialing external numbers | `object` |   | `false`
`external.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`external.number` | The caller id name for the object type | `string(0..35)` |   | `false`
`internal` | The default caller ID used when dialing internal extensions | `object` |   | `false`
`internal.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`internal.number` | The caller id name for the object type | `string(0..35)` |   | `false`

##### dialplans

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`system` | List of system dial plans | `array()` |   | `false`

##### metaflow

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### metaflow.audio_level

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.action` |   | `string` |   | `true`
`data.level` |   | `string` |   | `false`
`data.mode` |   | `string` |   | `false`
`module` |   | `string('audio_level')` |   | `true`

##### metaflow.break

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`module` |   | `string('break')` |   | `true`

##### metaflow.callflow

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.id` |   | `string` |   | `true`
`module` |   | `string('callflow')` |   | `true`

##### metaflow.hangup

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`module` |   | `string('hangup')` |   | `true`

##### metaflow.hold

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.moh_aleg` |   | `string` |   | `false`
`data.moh_bleg` |   | `string` |   | `false`
`data.unhold_key` |   | `string` | `1` | `false`
`module` |   | `string('hold')` |   | `true`

##### metaflow.hold_control

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`data.action` |   | `string('hold', 'unhold', 'toggle')` | `toggle` | `false`
`module` |   | `string('hold_control')` |   | `true`

##### metaflow.intercept

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.auto_answer` |   | `boolean` | `false` | `false`
`data.target_id` |   | `string` |   | `true`
`data.target_type` |   | `string('device', 'user', 'number')` |   | `true`
`data.unbridged_only` |   | `boolean` | `true` | `false`
`module` |   | `string('intercept')` |   | `true`

##### metaflow.move

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.auto_answer` |   | `boolean` | `false` | `false`
`data.device_id` |   | `string` |   | `false`
`data.owner_id` |   | `string` |   | `false`
`module` |   | `string('move')` |   | `true`

##### metaflow.play

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.id` |   | `string` |   | `true`
`data.leg` |   | `string('both', 'self', 'peer')` | `both` | `false`
`module` |   | `string('play')` |   | `true`

##### metaflow.record_call

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.action` |   | `string('start', 'stop', 'toggle')` | `toggle` | `true`
`data.format` |   | `string` |   | `false`
`data.media_name` |   | `string` |   | `false`
`module` |   | `string('record_call')` |   | `true`

##### metaflow.resume

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `false`
`module` |   | `string('resume')` |   | `true`

##### metaflow.say

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.language` |   | `string` |   | `false`
`data.method` |   | `string` |   | `false`
`data.text` |   | `string` |   | `true`
`data.type` |   | `string` |   | `false`
`module` |   | `string('say')` |   | `true`

##### metaflow.sound_touch

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.action` |   | `string('start', 'stop')` |   | `true`
`data.adjust_in_octaves` |   | `integer` |   | `false`
`data.adjust_in_semitones` |   | `integer` |   | `false`
`data.hook_dtmf` |   | `boolean` | `false` | `false`
`data.pitch` |   | `integer` |   | `false`
`data.rate` |   | `integer` |   | `false`
`data.sending_leg` |   | `boolean` | `false` | `false`
`data.tempo` |   | `integer` |   | `false`
`module` |   | `string('sound_touch')` |   | `true`

##### metaflow.transfer

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.Transfer-Type` |   | `string` | `attended` | `false`
`data.captures` |   | `string` |   | `false`
`data.target` |   | `string` |   | `false`
`module` |   | `string('transfer')` |   | `true`

##### metaflow.tts

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` |   | [#/definitions/metaflow_children](#metaflow_children) |   | `false`
`data` |   | `object` |   | `true`
`data.engine` |   | `string` | `flite` | `false`
`data.language` |   | `string` |   | `false`
`data.leg` |   | `string` | `self` | `false`
`data.terminators` |   | `string` |   | `false`
`data.text` |   | `string` |   | `true`
`data.voice` |   | `string` | `female` | `false`
`module` |   | `string('tts')` |   | `true`

##### metaflow_children

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------

##### metaflows

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '*', '#')` | `*` | `false`
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer` |   | `false`
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both', 'self', 'peer')` |   | `false`
`numbers` | A list of static numbers with their flows | `object` |   | `false`
`numbers./^[0-9]+$/` |   | [#/definitions/metaflow](#metaflow) |   | `false`
`patterns` | A list of patterns with their flows | `object` |   | `false`
`patterns./.+/` |   | [#/definitions/metaflow](#metaflow) |   | `false`

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

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{PHONE_NUMBER}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{PHONE_NUMBER}
```

