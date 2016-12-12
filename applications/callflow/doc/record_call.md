
The `record_call` callflow enables you to record the audio of the call.

## Mandatory fields

**action** - Must be set to `start` or `stop`.

## Optional fields

**time_limit** - Limit, in seconds, of how long to record the call. If ommited get value from `system_config/media/max_recording_time_limit`. Default `3600`.
**format** - Format to write the recording. Set `mp3` or `wav`. If ommited get value from `system_config/media/call_recording/extension`. Default `mp3`.
**record_on_answer** - Whether to delay starting the recording until the call is answered. Default `false`.
**record_sample_rate** - Sampling rate of the recording, in Hz. If ommited get value from `system_config/ecallmgr/record_sample_rate`. Default `8000`.
**record_min_sec** - Minimal record time, in seconds, to store recordings. If the recording time is less than this value, FreeSwitch will discard recorded file. If ommited get value from `system_config/media/record_min_sec`. Default `0`.
**url** - See **Storage of recordings** section.

## Storage of recordings

If you supply a URL in the `data` portion of the callflow, Kazoo will send an HTTP PUT with the recording to the URL when the recording has finished.

If you omit the URL, there are a couple options for storage:

* Check `system_config/media` for the `store_recordings` boolean()
    * If `false`, no storage will occur
    * If `true`, check `system_config/media` for `third_party_bigcouch_host`
        * If `undefined`, store to the Kazoo BigCouch cluster
        * If defined, send the recording to the configured URL
