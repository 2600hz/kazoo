## Set Variable

### About Set Variable

#### Schema

Validator for the set_variable callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`channel` | Which channel to set the variable on | `string('a' | 'both')` | `a` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`value` | The value to set 'variable' | `string()` |   | `false` |  
`variable` | The variable name to set on the leg(s) | `string()` |   | `false` |  



