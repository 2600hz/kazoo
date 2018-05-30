## Group Pickup

### About Group Pickup

Pickup a call in the group/user/device configured.

#### Schema

Validator for the group_pickup callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`approved_device_id` | Restrict calling device to Device | `string()` |   | `false` |
`approved_group_id` | Restrict calling device to Group | `string()` |   | `false` |
`approved_user_id` | Restrict calling device to User | `string()` |   | `false` |
`device_id` | Device to pickup | `string()` |   | `false` |
`group_id` | Group in which to find a call to pickup | `string()` |   | `false` |
`user_id` | User in which to find a call to pickup | `string()` |   | `false` |



### Usage

When a device is ringing in the office but no one is able to pick it up, it can be helpful to let others pick up the ringing line from their phone (versus hopping over desks and chairs, attempting to catch the call before it stops ringing).

#### Which devices are checked

The first thing to decide is the scope of the pickup group - it can be a single device, a user's device(s), or a group's device(s). You define the appropriate `device_id`, `user_id` or `group_id` in the action's data.

!!! note
    Preference is given to the most restrictive option if more than one are defined - so `device_id` is used before `user_id` is used before `group_id`.

#### Which devices can pickup ringing calls

It might not always be preferred to have anyone in the office able to call the `group_pickup` callflow. You can restrict the device, user, or group of users/devices who can utilize the callflow by defining the appropriate `approved_*` field (following similar preference which is used if multiple exist in the action's data).

### Example

Define a callflow with extension `8000` that allows the `sales` group's lines to be picked up by the `sales` group's devices:

```json
{"numbers":["8000"]
 ,"flow":{
   "module":"group_pickup"
   ,"data":{
     "group_id":"{SALES_GROUP_ID}"
     ,"approved_group_id":"{SALES_GROUP_ID}"
   }
 }
}
```

The `group_pickup` action is a terminal one; no children will be evaluated and can be omitted.
