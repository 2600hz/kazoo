## Hotdesk

### About Hotdesk

### Schema

Validator for the hotdesk callflow's data object

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | The action to take for hotdesking | `string('logout', 'login', 'toggle', 'bridge')` |   | `false`
`id` | Hotdesk ID | `string` |   | `false`
`interdigit_timeout` | How long, in seconds, to wait between keypresses | `integer` |   | `false`
