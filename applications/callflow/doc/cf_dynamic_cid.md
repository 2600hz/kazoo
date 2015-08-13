/*
Section: Callflows
Title: Dynamic change caller id
Language: en-US
Version: 4.0
*/

The `dynamic_cid` callflow enables you to change the caller id (CID).

## Mandatory fields

**action** - Must be set to `manual` or `list`.

If undefined, will revert to historical behavior, *manual*. 

## Optional fields

**time_limit** - Limit, in seconds, of how long to record the call. If ommited get value from `system_config/media/max_recording_time_limit`. Default `3600`.  
**format** - Format to write the recording. Set `mp3` or `wav`. If ommited get value from `system_config/media/call_recording/extension`. Default `mp3`.  
**record_on_answer** - Whether to delay starting the recording until the call is answered. Default `false`.  
**record_sample_rate** - Sampling rate of the recording, in Hz. If ommited get value from `system_config/ecallmgr/record_sample_rate`. Default `8000`.  
**record_min_sec** - Minimal record time, in seconds, to store recordings. If the recording time is less than this value, FreeSwitch will discard recorded file. If ommited get value from `system_config/media/record_min_sec`. Default `0`.  
**url** - See **Storage of recordings** section.

## Manual

Can only set the caller id number with this method.

## List

You can set the caller id number and the caller id name with this
method.

On a handset you dial `*2015149072508`

The callflow regex looks like this:
`
   "patterns": [
       "^\\*2(|[0-9]{2,})$"
	   ],
`

This means that `*2` is the "feature code" for this feature.

`01` is the entry in the `cidlist` document to use.

`5149072508` becomes `+15149072508` and gets dialed as such.

Example "list" CouchDB document.  This CouchDB doc will end up being
cached by Kazoo.  Make sure you flush changes..


```
{
   "_id": "cidlist",
   "_rev": "5-c0b5be0b4b65d51074ddb6808d075d6a",
   "entries": {
       "00": {
           "number": "16139999999",
           "name": "sssy co"
       },
       "01": {
           "number": "19058888888",
           "name": "bobs inc"
       }
   }
}
```



