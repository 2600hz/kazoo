## Missed Call Alert

### About Missed Call Alert

### Schema

Validator for the missed_call_alert callflow's data object

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`recipients` | One or more specific email addresses, Kazoo user ids or a combination of both | `array(object)` |   | `false`
`recipients.[].id` | The email address/user ID or the list of email addresses/user IDs based on sepecified type | `string` |   | `false`
`recipients.[].type` | Controls if the ID of this object is a Kazoo user ID or an email address | `string('user', 'email')` |   | `false`
