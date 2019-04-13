## Intercept

### About Intercept

#### Schema

Validator for the intercept callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`approved_device_id` | Restrict calling device to Device | `string()` |   | `false` |  
`approved_group_id` | Restrict calling device to Group | `string()` |   | `false` |  
`approved_user_id` | Restrict calling device to User | `string()` |   | `false` |  
`device_id` | Device to intercept | `string()` |   | `false` |  
`group_id` | Group in which to find a call to intercept | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`user_id` | User in which to find a call to intercept | `string()` |   | `false` |  



