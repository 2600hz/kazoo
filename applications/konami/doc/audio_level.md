## Audio Level

### About Audio Level

#### Schema

Allow changing the volume on a channel (including mute/unmute)



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Start or stop the audio adjustment | `string('start' | 'stop')` |   | `false`
`level` | Adjustment level for the audio | `integer()` |   | `false`
`mode` | What mode to use (depends on direction) | `string('read' | 'write')` |   | `false`



