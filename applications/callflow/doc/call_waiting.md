## Call Waiting

### About Call Waiting

Toggles call waiting on a device or user

#### Schema

Validator for the call_waiting callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform | `string('activate' | 'deactivate' | 'toggle')` | `toggle` | `false` |  
`scope` | Apply the action to the calling device or user | `string('device' | 'user')` | `device` | `false` |  



