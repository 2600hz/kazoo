## After Bridge

### About After Bridge

#### Schema

Validator for the after_bridge callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | What action to perform after a call is bridged | `string('park', 'transfer', 'hangup')` |   | `false`
`data` | The number to transfer to, or a boolean, depending on the 'action' | `string, boolean` |   | `false`


