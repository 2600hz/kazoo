# Sending Options

You can use a number of options when [sending requests](../guide/sending_requests.md) using the [request sending functions](sending_functions.md)

* [Common Options](#common-options)
* [Proxy Options](#proxy-options)
* [Header Manipulation Options](#header-manipulation-options)
* [Specific Options](#specific-options)

Keep in mind that activated plugins can add specific sending options. See [plugins documentation](../plugins/README.md).



## Common Options

Option|Types|Description|Commment
---|---|---|---|
{body, Body}|Body::`nksip:body()`|Sets the request body|
async||If present, the call will return inmediatly (instead of waiting for the final response) as `{async, ReqId}` (or `{error, Error}` if an error is produced before sending the request)|`ReqId` can be used with the functions in the [Request API](../api/requests.md) to get information about the request (the request may not be sent yet, so the information about transport may not be present)
callback|`fun/1`|If defined, it will be called for every received provisional response as `{reply, Code, Resp, Call}`. For `async` requests, it is called also for the final response and, if an error is produced before sending the request, as `{error, Error}`|Use the functions in the [Response API](../api/responses.md) to extract information. Do not spend time in this function, as it will block this call
get_request||If present, and the callback function is also present, if will be called when the request has been sent as `{req, Request, Call}`|You can use the functions in the [Request API](../api/requests.md) to extract relevant information from the request
meta|`[nksip_sipmsg:field()]`|Use it to select which specific fields from the final response shall be returned|Available fields are describe in [Metadata Fields](metadata.md)
{local_host, LocalHost}|LocalHost::`auto`&#124;`string()`&#124;`binary()`|Host or IP to use when auto generating headers like Contact or Record-Route
{local_host6, LocalHost}|LocalHost::`auto`&#124;`string()`&#124;`binary()`|Host or IP to use when auto generating headers like Contact or Record-Route using IPv6
user_agent||Automatically generates a User-Agent header, replacing any previous value
supported||Automatically generates a Supported header, replacing any previous value
allow||Automatically generates an Allow header, replacing any previous value
accept||Automatically generates an Accept header, replacing any previous value
date||Automatically generates a Date header, replacing any previous value
allow_event||Automatically generates an Allow-Event header, replacing any previous value
contact||Automatically generates a Contact header, if none is already present|Use it in dialog generaing requests as INVITE
no_dialog||Do not process dialogs for this request|
ignore||Ignore this option|


## Proxy Options

Option|Types|Description|Commment
---|---|---|---|
follow_redirects||If present, and a 3xx response is received, a new request will automatically be generated to the new redirected URI|Only for proxies
record_route||Automatically generates a Record-Route header, if none is already present|Used in proxies to force new in-dialog requests to pass through this proxy
path||Automatically generates a Path header, if none is already present|Used in proxies to force new registrations to go through this proxy

## Header Manipulation Options

Option|Types|Description|Commment
---|---|---|---|
{add, Name, Value}|Name::`nksip:header_name()`, Value::`nksip:header_value()`|Adds a new header, after any previous one with the same name|All header names should be lowercase
{add, {Name, Value}}|(same as before)|Same as before|
{replace, Name, Value}|(same as before)|Adds a new header, replacing any previous one|
{replace, {Name, Value}}|(same as before)|Same as before|
{insert, Name, Value}|(same as before)|Inserts a new header, before any previous one with the same name|
{insert, {Name, Value}}|(same as before)|Same as before|
{from, From}|From::`string()`&#124;`binary()`&#124;`nksip:uri()`|Replaces From header|Do not use in in-dialog requests
{to, From}|To::`string()`&#124;`binary()`&#124;`nksip:uri()`|Replces To header|Do not use in in-dialog requests
{call_id, CallId}|CallId::`binary()`|Replaces Call-ID header|Do not use in in-dialog requests
{content_type, ContentType}|ContentType::`string()`&#124;`binary()`&#124;`nksip:token()`|Replaces Content-Type header|
{require, Require}|Require::`string()`&#124;`binary()|Replaces Require header|
{supported, Supported}|Supported::`string()`&#124;`binary()`|Replaces Supported header
{expires, Expires}|Expires::`string()`&#124;`binary()`&#124;`integer()`|Replaces Expires header|
{contact, Contact}|Contact::`string()`&#124;`binary()`&#124;`nksip:uri()`&#124;`[nksip:uri()]`|Replaces Contact header|
{route, Route}|Route::`string()`&#124;`binary()`&#124;`nksip:uri()`&#124;`[nksip:uri()]`|Replaces Route header|
{reason, Reason}|Reason::`string()`&#124;`binary()`|Replaces Reason header|
{event, Reason}|Event::`string()`&#124;`binary()`&#124;`nksip:token()`|Replaces Event header|
{cseq_num, CSeq}|CSeq::`integer()`|Sets the numeric part of the CSeq header|Do not use in in-dialog requests|Useful for REFER

## Specific Options

Option|Types|Description|Commment
---|---|---|---|
auto_2xx_ack||Generates and sends an ACK automatically after a successful INVITE response|ACKs for unsucessful responses are always generated by NkSIP automatically
no_auto_expire||Do not generate automatic CANCEL for expired INVITE requests|
to_as_from||Replaces To header with current From value|Useful for REGISTER requests
unregister_all||For REGISTER requests, sets Contact to "*" and Expires to 0|Only for REGISTER
unregister||For REGISTER requests, sets `contact` and Expires to 0|Only for REGISTER
{subscription_state, ST}|see [notify/2](sending_functions.md#notify)|Only to be used in NOTIFY requests
{refer_to, Url}|Url::`string()`&#124;`binary()`&#124;`nksip:uri()`|Replaces Refer-To header|Only to be used in REFER requests
{sip_if_match, ETag}|ETag::`string()`&#124;`binary()`|Replaces SIP-If-Match header|Only to be used in PUBLISH requests


