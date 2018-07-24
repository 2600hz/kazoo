## Record Call

### About Record Call

#### Schema

Start a call recording



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Whether to start/stop the recording | `string('start' | 'stop' | 'toggle')` | `toggle` | `false`
`format` | What format to store the recording | `string('mp3' | 'wav')` |   | `false`
`label` | Add a custom label to the recording | `string()` |   | `false`
`media_name` | Name of the recording file | `string()` |   | `false`
`method` | HTTP method if using an HTTP destination | `string('put' | 'post')` |   | `false`
`origin` | Track how the recording was started | `string()` |   | `false`
`record_min_sec` | Minimum number of seconds recorded to consider it a valid recording | `integer()` |   | `false`
`record_on_answer` | Whether to start recording when the leg is answered | `boolean()` | `false` | `false`
`record_on_bridge` | Whether to start recording when the leg is bridged | `boolean()` | `false` | `false`
`record_sample_rate` | Sampling rate for the recording | `integer()` | `8000` | `false`
`time_limit` | How long to allow the recording, in seconds | `integer()` |   | `false`
`url` | HTTP URL to send the finished recording | `string()` |   | `false`



