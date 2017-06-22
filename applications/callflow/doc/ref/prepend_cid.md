## Prepend Cid

### About Prepend Cid

#### Schema

Validator for the prepend_cid callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Action to perform | `string('reset' | 'prepend')` | `prepend` | `false`
`apply_to` | Apply the prepend to which caller ID | `string('original' | 'current')` | `original` | `false`
`caller_id_name_prefix` | Prefix caller ID name | `string()` | "" | `false`
`caller_id_number_prefix` | Prefix caller ID number | `string()` | "" | `false`



