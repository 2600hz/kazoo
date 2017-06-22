## Group Pickup

### About Group Pickup

Pickup a call in the group/user/device configured.

#### Schema

Validator for the group_pickup callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`approved_device_id` | Restrict calling device to Device | `string()` |   | `false`
`approved_group_id` | Restrict calling device to Group | `string()` |   | `false`
`approved_user_id` | Restrict calling device to User | `string()` |   | `false`
`device_id` | Device to pickup | `string()` |   | `false`
`group_id` | Group in which to find a call to pickup | `string()` |   | `false`
`user_id` | User in which to find a call to pickup | `string()` |   | `false`



