## Prepend Cid

### About Prepend Cid

#### Schema

Validator for the prepend_cid callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | If the action parameter is "reset" the caller-id-name and caller-id-number will be restored to their original values, if the value is "prepend" then the prepending will happen | `string('reset' | 'prepend')` | `prepend` | `false` |  
`apply_to` | Either "original" or "current", this specifies that the prefix's should be applied to the current caller-id's or the original values | `string('original' | 'current')` | `current` | `false` |  
`caller_id_name_prefix` | The prefix that should be applied to the caller-id-name | `string()` | "" | `false` |  
`caller_id_number_prefix` | The prefix that should be applied to the caller-id-number | `string()` | "" | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



