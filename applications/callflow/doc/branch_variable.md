## Branch Variable

### About Branch Variable

Branch the callflow based on the configured variable (if it exists)

#### Schema

Validator for the branch_variable callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`scope` | specifies where the variable is defined | `string('account' | 'custom_channel_vars' | 'device' | 'merged' | 'user')` | `custom_channel_vars` | `false` |  
`variable` | specifies the name of variable/property that should be looked up | `string()` | "" | `true` |  



