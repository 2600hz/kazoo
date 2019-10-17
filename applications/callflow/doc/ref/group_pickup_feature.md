## Group Pickup Feature

### About Group Pickup Feature

#### Schema

Validator for the group_pickup_feature callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`type` | The type of collection to pickup | `string('group' | 'user' | 'device' | 'extension')` | `extension` | `true` |  



