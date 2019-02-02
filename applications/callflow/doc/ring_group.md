## Ring Group

### About Ring Group

Ring group callflow element allows calling multiple endpoints with given strategy and timeout.

#### Schema

Validator for the Ring Group callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`endpoints.[].delay` | How long to delay ringing the endpoint, in seconds | `integer()` | `0` | `false` |  
`endpoints.[].endpoint_type` | The type (device, user, etc) of endpoint | `string('device' | 'user' | 'group')` |   | `true` |  
`endpoints.[].id` | The ID of the endpoint | `string(1..128)` |   | `true` |  
`endpoints.[].timeout` | How long to ring the endpoint, in seconds | `integer()` | `20` | `false` |  
`endpoints.[].weight` | Weight of endpoint, different usage in various strategies | `integer()` |   | `false` |  
`endpoints` | Endpoint IDs (devices, users, etc) included in the ring group | `array(object())` |   | `true` |  
`fail_on_single_reject` | If any leg rejects the call, cancel all other legs | `boolean()` |   | `false` |  
`ignore_forward` | If true, will ignore SIP redirect requests for call-forwarded devices | `boolean()` | `true` | `false` |  
`repeats` | How many times to retry the ring group | `integer()` | `1` | `false` |  
`ringback` | Ringback to use | `string()` |   | `false` |  
`ringtones.external` | Ring tone for calls from external sources | `string()` |   | `false` |  
`ringtones.internal` | Ring tone for calls from external sources | `string()` |   | `false` |  
`ringtones` |   | `object()` |   | `false` |  
`strategy` | How to ring the members of the group | `string('simultaneous' | 'single' | 'weighted_random')` | `simultaneous` | `false` |  
`timeout` | How long to ring the ring group before continuing, in seconds | `integer()` | `20` | `false` |  






#### Strategy

There are three strategies that can be chosen:

* `single` - ring one endpoint after another
* `simultaneous` - ring all endpoints at the same time (default)
* `weighted_random` - randomize the list of endpoints, then use `single` strategy

#### Endpoints

Endpoints can be:
* `device` - a single device will be added to the `ring group`
* `user` - all devices of the user will be added to the `ring group`
* `group` - all devices of a [group](https://docs.2600hz.com/dev/applications/crossbar/doc/groups/) will be added to the `ring group`

Once all the endpoints have been resolved, the `strategy` will be applied to the resulting list of devices and the ring group will be executed.
