# Vmboxes

## About Vmboxes

#### Schema

Schema for a voicemail box



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`check_if_owner` | Determines if when the user calls their own voicemail they should be prompted to sign in | `boolean()` | `true` | `false` | `supported`
`delete_after_notify` | Move the voicemail to delete folder after the notification has been sent | `boolean()` | `false` | `false` | `supported`
`flags.[]` |   | `string()` |   | `false` | `supported`
`flags` | Flags set by external applications | `array(string())` |   | `false` | `supported`
`is_setup` | Determines if the user has completed the initial configuration | `boolean()` | `false` | `false` | `supported`
`is_voicemail_ff_rw_enabled` | callflow allow fastforward and rewind during voicemail message playback | `boolean()` | `false` | `false` |  
`mailbox` | The voicemail box number | `string(1..30)` |   | `true` | `supported`
`media.unavailable` | The ID of a media object that should be used as the unavailable greeting | `string(32)` |   | `false` | `supported`
`media` | The media (prompt) parameters | `object()` | `{}` | `false` | `supported`
`media_extension` | Voicemail audio format | `string('mp3' | 'mp4' | 'wav')` | `mp3` | `false` | `supported`
`name` | A friendly name for the voicemail box | `string(1..128)` |   | `true` | `supported`
`not_configurable` | Determines if the user can configure this voicemail. | `boolean()` | `false` | `false` | `supported`
`notify.callback` | Schema for a callback options | [#/definitions/notify.callback](#notifycallback) |   | `false` |  
`notify` |   | `object()` |   | `false` | `supported`
`notify_email_addresses.[]` |   | `string()` |   | `false` | `supported`
`notify_email_addresses` | List of email addresses to send notifications to (in addition to owner's email, if any) | `array(string())` | `[]` | `false` | `supported`
`oldest_message_first` | Play older voicemail messages before new ones | `boolean()` | `false` | `false` | `supported`
`operator_number` | Alternate/override number to use when calling operator from voicemail | `integer() | string()` |   | `false` |  
`owner_id` | The ID of the user object that 'owns' the voicemail box | `string(32)` |   | `false` | `supported`
`pin` | The pin number for the voicemail box | `string(4..6)` |   | `false` | `supported`
`require_pin` | Determines if a pin is required to check the voicemail from the users devices | `boolean()` | `false` | `false` | `supported`
`save_after_notify` | Move the voicemail to save folder after the notification has been sent (This setting will override delete_after_notify) | `boolean()` | `false` | `false` | `supported`
`seek_duration_ms` | callflow fastforward and rewind seek duration | `integer()` | `10000` | `false` |  
`skip_envelope` | Determines if the envelope should be skipped | `boolean()` | `false` | `false` | `beta`
`skip_greeting` | Determines if the greeting should be skipped | `boolean()` | `false` | `false` | `supported`
`skip_instructions` | Determines if the instructions after the greeting and prior to composing a message should be played | `boolean()` | `false` | `false` | `supported`
`timezone` | The default timezone | `string(5..32)` |   | `false` | `supported`
`transcribe` | Transcribe voicemail using ASR engine | `boolean()` | `false` | `false` | `alpha`

### notify.callback

Schema for a callback options


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attempts` | How many attempts without answer will system do | `integer()` |   | `false` |  
`disabled` | Determines if the system will call to callback number | `boolean()` |   | `false` |  
`interval_s` | How long will system wait between call back notification attempts | `integer()` |   | `false` |  
`number` | Number for callback notifications about new messages | `string()` |   | `false` |  
`schedule` | Schedules interval between callbacks | `array(integer())` |   | `false` |  
`timeout_s` | How long will system wait for answer to callback | `integer()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/messages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/messages
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/raw

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/raw
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw
```

