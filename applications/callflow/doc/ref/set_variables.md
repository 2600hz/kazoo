## Set Variables

### About Set Variables

#### Schema

Validator for the set_variables callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`custom_application_vars./[a-zA-Z0-9\-_]+/` |   | `string()` |   | `false`
`custom_application_vars` | Key-value pairs to set as custom_channel_vars on the channel | `object()` | `{}` | `false`
`export` | When true the provided custom_channel_vars are set on the channel and any channel bridged to it later | `boolean()` |   | `false`



