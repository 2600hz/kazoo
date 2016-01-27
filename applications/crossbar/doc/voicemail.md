
### Voicemail Boxes

#### About

Voicemail boxes store messages, recorded from the caller, for the voicemail box owner to listen to at a later time.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`check_if_owner` | Determines if when the user calls their own voicemail they should be prompted to sign in | `boolean` | `true` | `false`
`delete_after_notify` | Delete the voicemail after the notification has been sent | `boolean` | `false` | `false`
`is_setup` | Determines if the user has completed the initial configuration | `boolean` | `false` | `false`
`mailbox` | The voicemail box number | `string` |   | `true`
`media` | The media (prompt) parameters | `object` | `{}` | `false`
`media.unavailable` | The ID of a media object that should be used as the unavailable greeting | `string` |   | `false`
`messages` | The messages that have been left in the voicemail box | `array` | `[]` | `false`
`messages.[].call_id` | The SIP call-id | `string` |   | `false`
`messages.[].caller_id_name` | The reported caller id name | `string` |   | `false`
`messages.[].caller_id_number` | The reported caller id number | `string` |   | `false`
`messages.[].folder` | The folder the message belongs to | `string` |   | `false`
`messages.[].from` | The SIP from header | `string` |   | `false`
`messages.[].length` |   | `integer` |   | `false`
`messages.[].media_id` | The ID of the message media object | `string` |   | `false`
`messages.[].timestamp` | The UTC timestamp, in gregorian seconds, that the voicemail was left on | `integer` |   | `false`
`messages.[].to` | The SIP to header | `string` |   | `false`
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

#### Create a new Voicemail Box
