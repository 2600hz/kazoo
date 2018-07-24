## Group

### About Group

#### Schema

Validator for the group callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`endpoints.[]` |   | `string()` |   | `false` |  
`endpoints` | List of endpoint (device/user/group) IDs | `array(string())` | `[]` | `false` |  
`fail_on_single_reject` | If any leg rejects the call, cancel all other legs | `boolean()` |   | `false` |  
`id` | Group ID | `string()` |   | `false` |  
`ignore_forward` | Whether to ignore forwarded phones | `boolean()` | `true` | `false` |  
`repeats` | How many times to repeat dialing the group | `integer()` | `1` | `false` |  
`ringback` | Ringback to use | `string()` |   | `false` |  
`ringtones.external` | Ring tone for calls from external sources | `string()` |   | `false` |  
`ringtones.internal` | Ring tone for calls from external sources | `string()` |   | `false` |  
`ringtones` |   | `object()` |   | `false` |  
`strategy` | How to ring the endpoints | `string('single' | 'simultaneous')` | `simultaneous` | `false` |  
`timeout` | How long to attempt the group | `integer()` | `20` | `false` |  



