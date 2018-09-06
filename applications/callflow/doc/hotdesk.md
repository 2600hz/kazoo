## Hotdesk

### About Hotdesk

Dynamically assign a device to a user.

#### Schema

Validator for the hotdesk callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | The action to take for hotdesking | `string('logout' | 'login' | 'toggle' | 'bridge')` |   | `false` |  
`id` | Hotdesk ID | `string()` |   | `false` |  
`interdigit_timeout` | How long, in seconds, to wait between keypresses | `integer()` |   | `false` |  



