## Call Forward

### About Call Forward

Configure call forwarding for your device from your phone!

#### Schema

Validator for the call_forward callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform on the caller's call forwarding | `string('activate' | 'deactivate' | 'update' | 'toggle' | 'menu')` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



