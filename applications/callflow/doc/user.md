## User

### About User

Attempt to bridge the caller to all of a user's devices

#### Schema

Validator for the user callflow's data object



Key | Description | Type | Default | Required | Support
--- | ----------- | ---- | ------- | -------- | --------
`can_call_self` | Toggle whether devices of this use can call each other | `boolean()` |   | `false` | `supported`
`can_text_self` | Toggle whether devices of this use can text each other | `boolean()` |   | `false` | `alpha`
`delay` | How long to delay the endpoint(s) before bridging | `integer()` | `0` | `false` | `alpha`
`fail_on_single_reject` | If any leg rejects the call, cancel all other legs | `boolean()` |   | `false` | `alpha`
`id` | User ID | `string()` |   | `false` | `supported`
`static_invite` | Override the SIP Username | `string()` |   | `false` | `alpha`
`strategy` | Ringing strategy for the endpoint(s) | `string('simultaneous' | 'single')` | `simultaneous` | `false` | `alpha`
`suppress_clid` | Toggle whether to suppress caller ID | `boolean()` |   | `false` | `alpha`
`timeout` | Timeout, in seconds, to wait for answer | `integer()` | `20` | `false` | `supported`



