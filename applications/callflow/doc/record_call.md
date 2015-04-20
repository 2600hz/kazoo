/*
Section: Callflows
Title: Record Call
Language: en-US
Version: 3.20
*/

The `record_call` callflow enables you to record the audio of the call.

## Storage of recordings

If you supply a URL in the `data` portion of the callflow, Kazoo will send an HTTP PUT with the recording to the URL when the recording has finished.

If you omit the URL, there are a couple options for storage:

* Check `system_config/media` for the `store_recordings` boolean()
    * If `false`, no storage will occur
    * If `true`, check `system_config/media` for `third_party_bigcouch_host`
        * If `undefined`, store to the Kazoo BigCouch cluster
        * If defined, send the recording to the configured URL
