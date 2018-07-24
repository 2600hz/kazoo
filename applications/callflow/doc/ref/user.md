## User

### About User

#### Schema

Validator for the user callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`can_call_self` | Toggle whether devices of this use can call each other | `boolean()` |   | `false` |  
`can_text_self` | Toggle whether devices of this use can text each other | `boolean()` |   | `false` |  
`delay` | How long to delay the endpoint(s) before bridging | `integer()` | `0` | `false` |  
`fail_on_single_reject` | If any leg rejects the call, cancel all other legs | `boolean()` |   | `false` |  
`id` | User ID | `string()` |   | `false` |  
`static_invite` | Override the SIP Username | `string()` |   | `false` |  
`strategy` | Ringing strategy for the endpoint(s) | `string('simultaneous' | 'single')` | `simultaneous` | `false` |  
`suppress_clid` | Toggle whether to suppress caller ID | `boolean()` |   | `false` |  
`timeout` | Timeout, in seconds, to wait for answer | `integer()` | `20` | `false` |  



