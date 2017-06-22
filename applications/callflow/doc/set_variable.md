## Set Variable

### About Set Variable

Sets a variable on the current leg (useful in other callflow actions).

#### Schema

Validator for the set_variable callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`channel` | Which channel to set the variable on | `string('a' | 'both')` | `a` | `false`
`value` | The value to set 'variable' | `string()` |   | `false`
`variable` | The variable name to set on the leg(s) | `string()` |   | `false`



