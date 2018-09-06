# Eavesdrop Feature

### About Eavesdrop Feature

Use a feature code to determine what call to eavesdrop on

#### Schema

Validator for the eavesdrop_feature callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`approved_device_id` | Restricts the calling device to this Device | `string()` |   | `false` |  
`approved_group_id` | Requires the calling device to be part of the Group | `string()` |   | `false` |  
`approved_user_id` | Requires the calling device to be part of the User | `string()` |   | `false` |  
`group_id` | ID of the group/user/device to eavesdrop | `string()` |   | `false` |  



