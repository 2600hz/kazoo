## Record Caller

### About Record Caller

#### Schema

Record the caller's audio stream



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`format` | What format to store the recording on disk | `string('mp3' | 'wav')` |   | `false`
`method` | What HTTP method to use when sending the recording | `string('put' | 'post')` | `put` | `false`
`time_limit` | Time limit, in seconds, for the recording | `integer()` | `3600` | `false`
`url` | The URL to use when sending the recording for storage | `string()` |   | `false`



