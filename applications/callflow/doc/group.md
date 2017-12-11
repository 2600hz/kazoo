## Group

### About Group

Creates a group of endpoints to be rung when a call comes in.

#### Schema

Validator for the group callflow's data object



Key | Description | Type | Default | Required | Support
--- | ----------- | ---- | ------- | -------- | --------
`endpoints.[]` |   | `string()` |   | `false` | `Supported`
`endpoints` | List of endpoint (device/user/group) IDs | `array(string())` | `[]` | `false` | `supported`
`fail_on_single_reject` | If any leg rejects the call, cancel all other legs | `boolean()` |   | `false` | `alpha`
`id` | Group ID | `string()` |   | `false` | `Supported`
`ignore_forward` | Whether to ignore forwarded phones | `boolean()` | `true` | `false` | `alpha`
`repeats` | How many times to repeat dialing the group | `integer()` | `1` | `false` | `supported`
`ringback` | Ringback to use | `string()` |   | `false` | `Supported`
`ringtones.external` | Ring tone for calls from external sources | `string()` |   | `false` | `alpha`
`ringtones.internal` | Ring tone for calls from external sources | `string()` |   | `false` | `alpha`
`ringtones` |   | `object()` |   | `false` | `alpha`
`strategy` | How to ring the endpoints | `string('single' | 'simultaneous')` | `simultaneous` | `false` | `supported`
`timeout` | How long to attempt the group | `integer()` | `20` | `false` | `supported`



