## Park

### About Park

### Schema

Validator for the park callflow's data object

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Action to take for the caller | `string('park', 'retrieve', 'auto')` | `park` | `false`
`default_callback_timeout` | How long, in seconds, to wait before calling back the parker | `integer` |   | `false`
`default_presence_type` | Type of presence to update | `string` |   | `false`
`default_ringback_timeout` | How long, in milliseconds, before ringing back | `integer` |   | `false`
`slot` | Static slot number to use | `string` |   | `false`
`slots` | Statically define slots and their configuration | `object` | `null` | `false`
