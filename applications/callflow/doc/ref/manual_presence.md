## Manual Presence

### About Manual Presence

#### Schema

Validator for the Manual Presence callflow action



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`presence_id` | The Presence ID to send a presence notification about | `string()` |   | `true`
`status` | The status to update to | `string('idle' | 'ringing' | 'busy')` | `idle` | `false`



