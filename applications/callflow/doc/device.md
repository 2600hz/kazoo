## Device

### About Device

Bridge the caller to a device

#### Schema



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`can_call_self` | Toggle whether devices of the same owner can call each other | `boolean()` |   | `false` | `supported`
`can_text_self` | Toggle whether devices of the same owner can text each other | `boolean()` |   | `false` | `alpha`
`custom_sip_headers` | KV Object where the name is the SIP header and the value is the SIP field | `object()` |   | `false` |  
`delay` | How long to delay ringing the device, in seconds | `integer()` | `0` | `false` |  
`dial_strategy` | Dial strategy for the device | `string('simultaneous' | 'single')` | `simultaneous` | `false` |  
`id` | Device ID | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`static_invite` | Override the SIP Username | `string()` |   | `false` |  
`suppress_clid` | Suppress sending caller ID | `boolean()` |   | `false` |  
`timeout` | Time, in seconds, to wait for device to bridge | `integer()` | `0` | `false` |  



