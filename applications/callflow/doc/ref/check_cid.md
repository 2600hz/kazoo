## Check Cid

### About Check Cid

#### Schema

Validator for the check_cid callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`caller_id.external.name` | Update external Caller ID Name | `string()` |   | `false` |  
`caller_id.external.number` | Update external Caller ID Number | `string()` |   | `false` |  
`caller_id.external` |   | `object()` |   | `false` |  
`caller_id` |   | `object()` |   | `false` |  
`regex` | Determine match/nomatch when use_absolute_mode is false | `string()` | `.*` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`use_absolute_mode` | If true, direct call down a branch that matches the caller ID | `boolean()` | `false` | `false` |  
`user_id` | kazoo User ID to use as owner_id instead of detected owner_id | `string()` |   | `false` |  



