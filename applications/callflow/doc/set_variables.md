## Set Variables

### About Set Variables

#### Schema

Validator for the set_variables callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`custom_channel_vars./[a-zA-Z0-9\-_]+/` |   | `string()` |   | `false`
`custom_channel_vars` | Key-value pairs to set as custom_channel_vars on the channel | `object()` | `{}` | `true`
`export` | When true the provided custom_channel_vars are set on the channel and any channel bridged to it later | `boolean()` |   | `false`



