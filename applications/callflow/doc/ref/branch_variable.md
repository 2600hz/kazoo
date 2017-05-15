## Branch Variable

### About Branch Variable

#### Schema

Validator for the branch_variable callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`scope` | specifies where the variable is defined | `string('account', 'custom_channel_vars', 'device', 'merged', 'user')` | `custom_channel_vars` | `false`
`variable` | specifies the name of variable/property that should be looked up | `string` | "" | `true`


