## Device

### About Device

Bridge the caller to a device

#### Schema

Key | Description | Type | Default | Required | Support
--- | ----------- | ---- | ------- | -------- |--------
`can_call_self` | Toggle whether devices of the same owner can call each other | `boolean()` |   | `false` | `supported`
`can_text_self` | Toggle whether devices of the same owner can text each other | `boolean()` |   | `false` | `alpha`
`delay` | How long to delay ringing the device, in seconds | `integer()` | `0` | `false` | `alpha`
`id` | Device ID | `string()` |   | `false` | `supported`
`static_invite` | Override the SIP Username | `string()` |   | `false` | `alpha`
`suppress_clid` | Suppress sending caller ID | `boolean()` |   | `false` | `alpha`
`timeout` | Time, in seconds, to wait for device to bridge | `integer()` | `0` | `false` | `supported`


