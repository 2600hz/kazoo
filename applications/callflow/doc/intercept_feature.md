## Intercept Feature

### About Intercept Feature

Feature code to intercept a call (uses capture group to determine what the target is).

#### Schema

Validator for the intercept_feature callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`approved_device_id` | Restricts the calling device to this Device | `string()` |   | `false` |  
`approved_group_id` | Requires the calling device to be part of the Group | `string()` |   | `false` |  
`approved_user_id` | Requires the calling device to be part of the User | `string()` |   | `false` |  
`type` | The scope of devices to intercept | `string('user' | 'device' | 'extension')` |   | `false` |  



