## Device

### About Device

Bridge the caller to a device

#### Schema



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`can_call_self` | Toggle whether devices of the same owner can call each other | `boolean()` |   | `false`
`can_text_self` | Toggle whether devices of the same owner can text each other | `boolean()` |   | `false`
`delay` | How long to delay ringing the device, in seconds | `integer()` | `0` | `false`
`id` | Device ID | `string()` |   | `false`
`static_invite` | Override the SIP Username | `string()` |   | `false`
`suppress_clid` | Suppress sending caller ID | `boolean()` |   | `false`
`timeout` | Time, in seconds, to wait for device to bridge | `integer()` | `0` | `false`



