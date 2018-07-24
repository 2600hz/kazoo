## Eavesdrop

### About Eavesdrop

Eavesdrop on an in-progress call

#### Schema

Validator for the eavesdrop callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`approved_device_id` | Restricts the calling device to this Device | `string()` |   | `false` |  
`approved_group_id` | Requires the calling device to be part of the Group | `string()` |   | `false` |  
`approved_user_id` | Requires the calling device to be part of the User | `string()` |   | `false` |  
`device_id` | Device ID to eavesdrop | `string()` |   | `false` |  
`user_id` | User ID to eavesdrop | `string()` |   | `false` |  



