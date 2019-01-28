## Record Call

### About Record Call

The `record_call` callflow enables you to record the audio of the call (typically between two or more callers).

#### Schema

Validator for the Record Call callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | Whether to start or stop the recording | `string('start' | 'stop')` | `start` | `true` |  
`format` | What format to store the recording on disk | `string('mp3' | 'wav')` |   | `false` |  
`label` | Label to include in the origin of call recording | `string()` |   | `false` |  
`media_name` | the name of media | `string()` |   | `false` |  
`method` | HTTP verb to use when sending the recording to the supplied URL | `string('put' | 'post')` | `put` | `false` |  
`origin` | How the recording was started - read-only | `string()` |   | `false` |  
`record_min_sec` | The minimum length, in seconds, the recording must be to be considered successful. Otherwise it is deleted | `integer()` |   | `false` |  
`record_on_answer` | Whether to delay the recording until the channel is answered | `boolean()` | `false` | `false` |  
`record_on_bridge` | Whether to delay the recording until the channel is bridged | `boolean()` | `false` | `false` |  
`record_sample_rate` | What sampling rate to use on the recording | `integer()` |   | `false` |  
`should_follow_transfer` | If true, the recording will continue after a transfer on the active leg | `boolean()` | `true` | `false` |  
`time_limit` | Time limit, in seconds, for the recording | `integer()` | `3600` | `false` |  
`url` | The URL to use when sending the recording for storage | `string()` |   | `false` |  






## Storage of recordings

If you supply a URL in the `data` portion of the callflow, Kazoo will send an HTTP PUT with the recording to the URL when the recording has finished.

If you omit the URL, there are a couple options for storage:

* Check `system_config/media` for the `store_recordings` boolean()
    * If `false`, no storage will occur
    * If `true`, check `system_config/media` for `third_party_bigcouch_host`
        * If `undefined`, store to the Kazoo BigCouch cluster
        * If defined, send the recording to the configured URL
