## Record Caller

### About Record Caller

Record the media stream, sending it to a configured URL on call completion.

#### Schema

Record the caller's audio stream



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`format` | What format to store the recording on disk | `string('mp3' | 'wav')` |   | `false` |  
`method` | What HTTP method to use when sending the recording | `string('put' | 'post')` | `put` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`time_limit` | Time limit, in seconds, for the recording | `integer()` | `3600` | `false` |  
`url` | The URL to use when sending the recording for storage | `string()` |   | `false` |  



