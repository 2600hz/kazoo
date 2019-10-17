## Group Pickup Feature

### About Group Pickup Feature

#### Schema

Validator for the group_pickup_feature callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`approved_device_id` | Restrict calling device to Device | `string()` |   | `false` |  
`approved_group_id` | Restrict calling device to Group | `string()` |   | `false` |  
`approved_user_id` | Restrict calling device to User | `string()` |   | `false` |  
`device_id` | Device to pickup | `string()` |   | `false` |  
`group_id` | Group in which to find a call to pickup | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`type` | The type of collection to pickup | `string('group' | 'user' | 'device' | 'extension')` | `extension` | `true` |  
`user_id` | User in which to find a call to pickup | `string()` |   | `false` |  



