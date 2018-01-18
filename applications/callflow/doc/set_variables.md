## Set Variables

### About Set Variables

#### Schema

Validator for the set_variables callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_application_vars./[a-zA-Z0-9\-_]+/` |   | `string()` |   | `false` |  
`custom_application_vars` | Key-value pairs to set as custom_application_vars on the channel | `object()` | `{}` | `true` |  
`export` | When true the provided custom_application_vars are set on the channel and any channel bridged to it later | `boolean()` |   | `false` |  



