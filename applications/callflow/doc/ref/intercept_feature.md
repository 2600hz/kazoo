## Intercept Feature

### About Intercept Feature

#### Schema

Validator for the intercept_feature callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`approved_device_id` | Restricts the calling device to this Device | `string()` |   | `false`
`approved_group_id` | Requires the calling device to be part of the Group | `string()` |   | `false`
`approved_user_id` | Requires the calling device to be part of the User | `string()` |   | `false`
`type` | The scope of devices to intercept | `string('user' | 'device' | 'extension')` |   | `false`



