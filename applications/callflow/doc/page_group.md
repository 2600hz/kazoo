## Page Group

### About Page Group

Creates a page group from devices/users/groups

#### Schema

Validator for the page_group callflow's data object



Key | Description | Type | Default | Required | Support
--- | ----------- | ---- | ------- | -------- | --------
`audio` | What kind of audio for the group members | `string('one-way' | 'two-way')` | `one-way` | `true` | 
`barge_calls` | Should the Endpoint be interrupted by this call | `boolean()` |   | `false` | 
`endpoints.[].delay` | How long to delay ringing the endpoint, in seconds | `integer()` | `0` | `false` | 
`endpoints.[].endpoint_type` | The type (device, user, etc) of endpoint | `string('device' | 'user' | 'group')` |   | `true` | 
`endpoints.[].id` | The ID of the endpoint | `string(1..128)` |   | `true` | 
`endpoints.[].timeout` | How long to ring the endpoint, in seconds | `integer()` | `20` | `false` | 
`endpoints.[].weight` | Weight of endpoint, different usage in various strategies | `integer()` |   | `false` | 
`endpoints` | Endpoint IDs (devices, users, etc) included in the ring group | `array(object())` |   | `true` |
`timeout` | How long to ring the ring group before continuing, in seconds | `integer()` | `5` | `false` |



