## Record Call

### About Record Call

#### Schema

Validator for the Record Call callflow action



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Whether to start or stop the recording | `string('start', 'stop')` | `start` | `true`
`format` | What format to store the recording on disk | `string('mp3', 'wav')` |   | `false`
`media_name` | the name of media | `string` |   | `false`
`record_min_sec` | The minimum length, in seconds, the recording must be to be considered successful. Otherwise it is deleted | `integer` |   | `false`
`record_on_answer` | Whether to delay the recording until the channel is answered | `boolean` | `false` | `false`
`record_on_bridge` | Whether to delay the recording until the channel is bridged | `boolean` | `false` | `false`
`record_sample_rate` | What sampling rate to use on the recording | `integer` |   | `false`
`time_limit` | Time limit, in seconds, for the recording | `integer` | `3600` | `false`
`url` | The URL to use when sending the recording for storage | `string` |   | `false`


