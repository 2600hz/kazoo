## Route To Cid

### About Route To Cid

#### Schema

Endpoints lookup by cid number



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`can_call_self` | Toggle whether devices of the same owner can call each other | `boolean()` |   | `false` |  
`can_text_self` | Toggle whether devices of the same owner can text each other | `boolean()` |   | `false` |  
`cid_types.[]` |   | `string()` |   | `false` |  
`cid_types` | CID types to perform search: internal, external, custom | `array(string())` | `["internal"]` | `false` |  
`delay` | How long to delay ringing the device, in seconds | `integer()` | `0` | `false` |  
`endpoint_types.[]` |   | `string()` |   | `false` |  
`endpoint_types` | Endpoint types to perform search: user, device | `array(string())` | `[]` | `false` |  
`static_invite` | Override the SIP Username | `string()` |   | `false` |  
`suppress_clid` | Suppress sending caller ID | `boolean()` |   | `false` |  
`timeout` | Time, in seconds, to wait for device to bridge | `integer()` | `0` | `false` |  



