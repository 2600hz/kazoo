## Ring Group Toggle

### About Ring Group Toggle

#### Schema

Validator for the ring_group_toggle callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What the module should do. Options are 'login' and 'logout' | `string('login' | 'logout')` |   | `true` |  
`callflow_id` | The callflow containing the ring group to log in and out of | `string()` |   | `true` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



