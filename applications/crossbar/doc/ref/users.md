### Users

#### About Users

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_forward` | The device call forward parameters | `object` |   | `false`
`call_forward.direct_calls_only` | Determines if the calls that are not directly sent to the device should be forwarded | `boolean` | `false` | `false`
`call_forward.enabled` | Determines if the call forwarding should be used | `boolean` | `false` | `false`
`call_forward.failover` | Enable the call-forwarding parameters if the device is offline | `boolean` | `false` | `false`
`call_forward.ignore_early_media` | The option to determine if early media from the call forwarded number should ignored | `boolean` | `true` | `false`
`call_forward.keep_caller_id` | Determines if the caller id is kept when the call is forwarded, if not the devices caller id is used | `boolean` | `true` | `false`
`call_forward.number` | The number to forward calls to | `string` |   | `false`
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
`email` | The email of the user | `string` |   | `false`
`enabled` | Determines if the user is currently enabled | `boolean` | `true` | `false`
`first_name` | The first name of the user | `string` |   | `true`
`hotdesk` | The user hotdesk parameters | `object` | `{}` | `false`
`hotdesk.enabled` | Determines if the user has hotdesking enabled | `boolean` | `false` | `false`
`hotdesk.id` | The users hotdesk id | `string` |   | `false`
`hotdesk.keep_logged_in_elsewhere` | Determines if user should be able to login to mutliple phones simultaneously | `boolean` | `false` | `false`
`hotdesk.pin` | The users hotdesk pin number | `string` |   | `false`
`hotdesk.require_pin` | Determines if user requires a pin to change the hotdesk state | `boolean` | `false` | `false`
`language` | The language for this user | `string` |   | `false`
`last_name` | The last name of the user | `string` |   | `true`
`media` | The device media parameters | `object` | `{}` | `false`
`media.audio` | The audio media parameters | `object` | `{}` | `false`
`media.audio.codecs` | A list of audio codecs the device supports | `array` | `PCMU` | `false`
`media.audio.codecs.[]` |   | `string` |   | `false`
`media.bypass_media` | Default bypass media mode | `booleanstring` |   | `false`
`media.encryption` |   | `object` | `{}` | `false`
`media.encryption.enforce_security` |   | `boolean` | `false` | `false`
`media.encryption.methods` |   | `array` | `[]` | `false`
`media.encryption.methods.[]` |   | `string` |   | `false`
`media.fax_option` | Support T.38 | `boolean` |   | `false`
`media.ignore_early_media` | The option to determine if early media from the device should always be ignored | `boolean` |   | `false`
`media.progress_timeout` | The progress timeout to apply to the device | `integer` |   | `false`
`media.video` | The video media parameters | `object` | `{}` | `false`
`media.video.codecs` | A list of video codecs the device supports | `array` | `[]` | `false`
`media.video.codecs.[]` |   | `string` |   | `false`
`metaflows` | The device metaflow parameters |   |   | `false`
`music_on_hold` | The music on hold parameters used if not a property of the device owner | `object` | `{}` | `false`
`music_on_hold.media_id` | The ID of a media object that should be used as the music on hold | `string` |   | `false`
`presence_id` | User's presence ID | `string` |   | `false`
`priv_level` | The privilege level of the user | `string` | `user` | `false`
`profile` | User's profile data | `object` | `{}` | `false`
`pronounced_name` | Name pronounced by user to introduce himself to conference members | `object` |   | `false`
`pronounced_name.media_id` | The ID of a media object that should be used as the music on hold | `string` |   | `false`
`require_password_update` | UI flag that the user should update their password. | `boolean` | `false` | `false`
`ringtones` |   | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string` |   | `false`
`timezone` | User's timezone | `string` |   | `false`
`username` | The GUI login username - alpha-numeric, dashes, at symbol, periods, plusses, and underscores allowed | `string` |   | `false`
`verified` | Determines if the user has been verified | `boolean` | `false` | `false`
`vm_to_email_enabled` | Determines if the user would like voicemails emailed to them | `boolean` | `true` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/users

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/users

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/users/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/users/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/users/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/users/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/users/{ID}/vcard

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}/vcard
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/users/{ID}/photo

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}/photo
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/users/{ID}/photo

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}/photo
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/users/{ID}/photo

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}/photo
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/users/{ID}/quickcall/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/users/{ID}/quickcall/{ID}
```

