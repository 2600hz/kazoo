## Branch Variable

### About Branch Variable

#### Schema

Validator for the branch_variable callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`scope` | specifies where the variable is defined | `string('account' | 'custom_channel_vars' | 'device' | 'merged' | 'user')` | `custom_channel_vars` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`variable` | specifies the name of variable/property that should be looked up | `string()` | "" | `true` |  



