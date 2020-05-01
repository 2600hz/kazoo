## Manual Presence

### About Manual Presence

#### Schema

Validator for the Manual Presence callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`presence_id` | The Presence ID to send a presence notification about | `string()` |   | `true` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`status` | The status to update to | `string('idle' | 'ringing' | 'busy')` | `idle` | `false` |  






#### Status

There are three statuses that may be used in the update:

* `idle` - Typically solid green, for when the `presence_id` has no active calls
* `ringing` - Typically blinking red, for when an incoming call is occurring
* `busy` - Typically solid red, for when an incoming call has been answered
