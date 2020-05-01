## Manual Presence

### About Manual Presence

#### Schema

Validator for the Manual Presence callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`presence_id` | The Presence ID to send a presence notification about | `string()` |   | `true` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`status` | The status to update to | `string('idle' | 'ringing' | 'busy')` | `idle` | `false` |  



