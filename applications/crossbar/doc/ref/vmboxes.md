### Vmboxes

#### About Vmboxes

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`check_if_owner` | Determines if when the user calls their own voicemail they should be prompted to sign in | `boolean` | `true` | `false`
`delete_after_notify` | Delete the voicemail after the notification has been sent | `boolean` | `false` | `false`
`is_setup` | Determines if the user has completed the initial configuration | `boolean` | `false` | `false`
`mailbox` | The voicemail box number | `string(1..30)` |   | `true`
`media` | The media (prompt) parameters | `object` | `{}` | `false`
`media.unavailable` | The ID of a media object that should be used as the unavailable greeting | `string(32)` |   | `false`
`name` | A friendly name for the voicemail box | `string(1..128)` |   | `true`
`not_configurable` | Determines if the user can configure this voicemail. | `boolean` | `false` | `false`
`notify` |   | `object` |   | `false`
`notify.callback` |   | `#/definitions/notify.callback` |   | `false`
`notify_email_addresses` | List of email addresses to send notifications to (in addition to owner's email, if any) | `array(string)` | `[]` | `false`
`notify_email_addresses.[]` |   | `string` |   | `false`
`owner_id` | The ID of the user object that 'owns' the voicemail box | `string(32)` |   | `false`
`pin` | The pin number for the voicemail box | `string(4..15)` |   | `false`
`require_pin` | Determines if a pin is required to check the voicemail from the users devices | `boolean` | `false` | `false`
`save_after_notify` | Save the voicemail after the notification has been sent | `boolean` | `false` | `false`
`skip_greeting` | Determines if the greeting should be skipped | `boolean` | `false` | `false`
`skip_instructions` | Determines if the instructions after the greeting and prior to composing a message should be played | `boolean` | `false` | `false`
`timezone` | The default timezone | `string(5..32)` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}/raw

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}/raw
```
