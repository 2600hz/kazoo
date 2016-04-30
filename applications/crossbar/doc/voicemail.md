### Voicemail Boxes

#### About Voicemail Boxes

Voicemail boxes store messages, recorded from the caller, for the voicemail box owner to listen to at a later time.

#### Differences between version 1 and 2 of Voicemail box

As of Kazoo 4.0 all new voicemail messages will be stored in the account MODbs.

Regarding this change the v2 vmboxes API will ___no longer___ returns the messages array. For compatiblity issues v1 vmboxes API tries to return the messages array along the vmboxes API response, but because of how things work behind this moving to MODBs, manipulation the messages array on the v1 is ___strongly___ discuraged.

The existing messages API _should be_ used to manage messages in a voicemail box for _both_ versions.

For more information about voicemail changes see documentation for kazoo_voicemail.

#### Voicemail box Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`check_if_owner` | Determines if when the user calls their own voicemail they should be prompted to sign in | `boolean` | `true` | `false`
`delete_after_notify` | Delete the voicemail after the notification has been sent | `boolean` | `false` | `false`
`is_setup` | Determines if the user has completed the initial configuration | `boolean` | `false` | `false`
`mailbox` | The voicemail box number | `string` |   | `true`
`media` | The media (prompt) parameters | `object` | `{}` | `false`
`media.unavailable` | The ID of a media object that should be used as the unavailable greeting | `string` |   | `false`
`name` | A friendly name for the voicemail box | `string` |   | `true`
`not_configurable` | Determines if the user can configure this voicemail. | `boolean` | `false` | `false`
`notify_email_addresses` | List of email addresses to send notifications to (in addition to owner's email, if any) | `array` | `[]` | `false`
`notify_email_addresses.[]` |   | `string` |   | `false`
`owner_id` | The ID of the user object that 'owns' the voicemail box | `string` |   | `false`
`pin` | The pin number for the voicemail box | `string` |   | `false`
`require_pin` | Determines if a pin is required to check the voicemail from the users devices | `boolean` | `false` | `false`
`save_after_notify` | Save the voicemail after the notification has been sent | `boolean` | `false` | `false`
`skip_greeting` | Determines if the greeting should be skipped | `boolean` | `false` | `false`
`skip_instructions` | Determines if the instructions after the greeting and prior to composing a message should be played | `boolean` | `false` | `false`
`timezone` | The default timezone | `string` |   | `false`


#### Mailbox Message schema

The messages that have been left in the voicemail box.

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_id` | The SIP call-id | `string` |   | `false`
`caller_id_name` | The reported caller id name | `string` |   | `false`
`caller_id_number` | The reported caller id number | `string` |   | `false`
`folder` | The folder the message belongs to | `string` |   | `false`
`from` | The SIP from header | `string` |   | `false`
`length` |   | `integer` |   | `false`
`media_id` | The ID of the message object | `string` |   | `false`
`timestamp` | The UTC timestamp, in gregorian seconds, that the voicemail was left on | `integer` |   | `false`
`to` | The SIP to header | `string` |   | `false`

#### Fetch all the voicemail boxes for the account

> GET /v2/accounts/{ACCOUNTID}/vmboxes

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes
```

#### Create a new voicemail box

> PUT /v2/accounts/{ACCOUNTID}/vmboxes

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes
```

#### Remove a voicemail box

> DELETE /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}
```

#### Fetch a voicemail box

> GET /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}
```

#### Patch a voicemail box

> PATCH /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}
```

#### Change a voicemail box's settings

> POST /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}
```

#### Remove all messages from a voicemail box

> DELETE /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages
```

#### Fetch all messages for a voicemail box

> GET /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages
```

#### Remove a message from the voicemail box

> DELETE /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}
```

#### Fetch a message from the voicemail box

> GET /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}
```

#### Change a message from a voicemail box

> POST /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}
```

#### Fetch the raw audio of the message

> GET /v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}/raw

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/vmboxes/{VMBOXID}/messages/{MSGID}/raw
```
