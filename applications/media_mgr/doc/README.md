# Media Manager

Receives requests to resolve media IDs into HTTP URLs. Could be for prompt/media URLs for FreeSWITCH to play to the caller or URLs for FreeSWITCH to use to upload recordings, faxes, etc.

Media manager also receives requests to generate TTS media files, caching the resulting media for FreeSWITCH to stream.

## Rough Architecture

Single play stream

ECALLMGR         MEDIA MGR
media_req    --> media_listener
                   verify media exists
                   generate URL to SHOUT server
call_control <--   media_resp
                   prefetch attachment if not cached locally


FS
playback() -->   media_shout_protocol
                   accept client
                   verify stream path exists (can issue redirect to couch?)
                   send chunked data to socket
