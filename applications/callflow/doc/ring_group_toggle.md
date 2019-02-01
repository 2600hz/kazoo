## Ring Group Toggle

### About Ring Group Toggle

Ring Group Toggle allows a user to log in and out of a ring group dynamically.

#### Schema

Validator for the ring_group_toggle callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What the module should do. Options are 'login' and 'logout' | `string('login' | 'logout')` |   | `true` |  
`callflow_id` | The callflow containing the ring group to log in and out of | `string()` |   | `true` |  






### Details

The `{ACTION}` is what the callflow element should do. Possible values are `login` and `logout`.
`login` will set the `disable_until` field to `0` on the user's endpoint for the ring group
found in the callflow specified by `CALLFLOW_ID`. This will have the effect of logging the user
into the ring group. `logout` will set the `disable_until` field to a time well in the future,
having the effect of logging the user out.
