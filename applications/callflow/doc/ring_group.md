## Ring Group

### About Ring Group

Ring group callflow element allows calling multiple endpoints with given strategy and timeout.

### Schema

Validator for the Ring Group callflow element

Key | Description | Type | Default | Required | Support
--- | ----------- | ---- | ------- | -------- | --------
`endpoints` | Endpoint IDs (devices, users, etc) included in the ring group | `array(object)` | `[]` | `true` | `supported`
`endpoints.[].delay` | How long to delay ringing the endpoint, in seconds | `integer` | `0` | `false` | `supported`
`endpoints.[].endpoint_type` | The type (device, user, etc) of endpoint | `string('device', 'user', 'group')` |   | `true` | `supported`
`endpoints.[].id` | The ID of the endpoint | `string(1..128)` |   | `true` | `supported`
`endpoints.[].timeout` | How long to ring the endpoint, in seconds | `integer` | `20` | `false` |`supported`
`endpoints.[].weight` | Weight of endpoint, different usage in various strategies | `integer` |   | `false` | `alpha`
`fail_on_single_reject` | If any leg rejects the call, cancel all other legs | `boolean` |   | `false` |
`ignore_forward` | If true, will ignore SIP redirect requests for call-forwarded devices | `boolean` | `true` | `false` | `alpha`
`repeats` | How many times to retry the ring group | `integer` | `1` | `false` | `supported`
`ringback` | Ringback to use | `string` |   | `false` | `supported`
`ringtones` |   | `object` |   | `false` | `supported`
`ringtones.external` | Ring tone for calls from external sources | `string` |   | `false` | `supported`
`ringtones.internal` | Ring tone for calls from external sources | `string` |   | `false` | `supported`
`strategy` | How to ring the members of the group | `string('simultaneous', 'single', 'weighted_random')` | `simultaneous` | `false` | `supported`
`timeout` | How long to ring the ring group before continuing, in seconds | `integer` | `20` | `false` | `supported`



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
