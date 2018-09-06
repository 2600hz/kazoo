
# Media Manager
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
