## After Bridge

### About After Bridge

#### Schema

Validator for the after_bridge callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform after a call is bridged | `string('park' | 'transfer' | 'hangup')` |   | `false` |  
`data` | The number to transfer to, or a boolean, depending on the 'action' | `string() | boolean()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



