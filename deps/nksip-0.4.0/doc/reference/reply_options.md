# Reply Options

Many of the SipApp's [callback functions](callback_functions.md) allows you to send a response back for the received request. Currently recognized options are documented in this page.

Keep in mind that activated pluings can add new options. See the [plugins documentation](../plugins/README.md).

Response|Types|Code|Comments
---|---|---|---
Code|Code::`nksip:sip_code()`|Code|
{Code, Opts}|Code::`nksip:sip_code()`, Opts::`nksip:optslist()`|Code|See [options](#options)
ringing||180|
rel_ringing||180|_Reliable responses_ will be used (must activate [plugin](../plugins/100rel.md))
{rel_ringing, Body}|Body::`nksip:body()`|180|_Reliable responses_ will be used, send a body (must activate [plugin](../plugins/100rel.md))
session_progress||183|
rel_session_progress||183|_Reliable responses_ will be used (must activate [plugin](../plugins/100rel.md))
{rel_session_progress, Body}|Body:`nksip:body()`|183|_Reliable responses_ will be used, send a body (must activate [plugin](../plugins/100rel.md))
ok||200|
{ok, Opts}|Opts:`nksip:optslist()`|200|See [options](#options)
{answer, Body}|Body::`nksip:body()`|200|Send a body
accepted||202|
{redirect, [Contact]}|Contact::`string()`&#124;`binary()`&#124;`nksip:uri()`|300|Generates _Contact_ headers
{redirect_permanent, Contact}|Contact::`string()`&#124;`binary()`&#124;`nksip:uri()`|301|Generates a _Contact_ header
{redirect_temporary, Contact}|Contact::`string()`&#124;`binary()`&#124;`nksip:uri()`|302|Generates a _Contact_ header
invalid_request||400|
{invalid_request, Phrase}|Phrase::`string()`&#124;`binary()`|400|Use custom SIP phrase
authenticate||401|Generates a new _WWW-Authenticate_ header, using current _From_ domain as `realm`
{authenticate, Realm}|Realm::`string()`&#124;`binary()`|401|Generates a valid new _WWW-Authenticate header_, using `Realm`
forbidden||403|
{forbidden, Text}|Text::`string()`&#124;`binary()`|403|Use custom SIP phrase
not_found||404|
{not_found, Text}|Text::`string()`&#124;`binary()`|404|Use custom SIP phrase
{method_not_allowed, Allow}|Allow::`string()`&#124;`binary()`|405|Generates an _Allow_ header
proxy_authenticate||407|Generates a valid new _Proxy-Authenticate header_, using current _From_ domain as `Realm`
{proxy_authenticate, Realm}|Realm::`string()`&#124;`binary()`|407|Generates a valid new _Proxy-Authenticate header_, using `Realm`
timeout||408|
{timeout, Text}|Text::`string()`&#124;`binary()`|408|Use custom SIP phrase
conditional_request_failed||412|
request_too_large||413|
{unsupported_media_type, Accept}|Accept::`string()`&#124;`binary()`|415|Generates a new _Accept_ header
{unsupported_media_encoding, AcceptEncoding}|AcceptEncoding::`string()`&#124;`binary()`|415|Generates a new _Accept-Encoding_ header
unsupported_uri_scheme||416|
{bad_extension, Unsupported}|Unsupported::`string()`&#124;`binary()`|420|Generates a new _Unsupported_ header
{extension_required, Require}|Require::`string()`&#124;`binary()`|421|Generates a new _Require_ header
{interval_too_brief, MinExpires}|MinExpires::`integer()`|423|Generates a new _Min-Expires_
flow_failed||430|
first_hop_lacks_outbound||439|
temporarily_unavailable||480|
no_transaction||481|
unknown_dialog||481|
loop_detected||482|
too_many_hops||483|
ambiguous||485|
busy||486|
request_terminated||487|
{not_acceptable, Warning}|Warning::`string()`&#124;`binary()`|488|Generates a new _Warning_ header
bad_event||489|
request_pending||491|
internal_error||500|
{internal_error, Text}|Text::`string()`&#124;`binary()`|500|Use custom SIP phrase
busy_eveywhere||600|
decline||603|

## Options

Some previous replies allow including options. The recognized options are:

Option|Types|Description|Commment
---|---|---|---|
{body, Body}|Body::`nksip:body()`|Sets the request body|
{reason_phrase, Phrase}|Phrase::`string()`&#124;`binary`|Uses a custom reason phrase in the SIP response|
{local_host, LocalHost}|LocalHost::`auto`&#124;`string()`&#124;`binary()`|Host or IP to use when auto generating headers like Contact or Record-Route
{local_host6, LocalHost}|LocalHost::`auto`&#124;`string()`&#124;`binary()`|Host or IP to use when auto generating headers like Contact or Record-Route using IPv6
user_agent|-|Automatically generates a User-Agent header, replacing any previous value
supported|-|Automatically generates a Supported header, replacing any previous value
allow|-|Automatically generates an Allow header, replacing any previous value
accept|-|Automatically generates an Accept header, replacing any previous value
allow_event|-|Automatically generates an Allow-Event header, replacing any previous value
contact|-|Automatically generates a Contact header, if none is already present|Use it in dialog generaing requests as INVITE
www_authenticate||Generates an automatic WWW-Authenticate header, using From header as _realm_
{www_authenticate, Realm}|Realm::`string()`&#124;`binary`|Generates an automatic WWW-Authenticate header, using _Realm_
proxy_authenticate||Generates an automatic Proxy-Authenticate header, using From header as _realm_
{proxy_authenticate, Realm}|Realm::`string()`&#124;`binary`|Generates an automatic Proxy-Authenticate header, using _Realm_
{service_route, Routes}|Routes::`string()`&#124;`binary`&#124;`nksip:uri()`|For REGISTER requests, if code is in the 200-299 range, generates a Service-Route header
{add, Name, Value}|Name::`nksip:header_name()`, Value::`nksip:header_value()`|Adds a new header, after any previous one with the same name|All header names should be lowercase
{add, {Name, Value}}|(same as before)|Same as before|
{replace, Name, Value}|(same as before)|Adds a new header, replacing any previous one|
{replace, {Name, Value}}|(same as before)|Same as before|
{insert, Name, Value}|(same as before)|Inserts a new header, before any previous one with the same name|
{insert, {Name, Value}}|(same as before)|Same as before|
{content_type, ContentType}|ContentType::`string()`&#124;`binary()`&#124;`nksip:token()`|Replaces Content-Type header|
{require, Require}|Require::`string()`&#124;`binary()|Replaces Require header|
{supported, Supported}|Supported::`string()`&#124;`binary()`|Replaces Supported header
{expires, Expires}|Expires::`string()`&#124;`binary()`&#124;`integer()`|Replaces Expires header|
{contact, Contact}|Contact::`string()`&#124;`binary()`&#124;`nksip:uri()`&#124;`[nksip:uri()]`|Replaces Contact header|
{route, Route}|Route::`string()`&#124;`binary()`&#124;`nksip:uri()`&#124;`[nksip:uri()]`|Replaces Route header|
{reason, Reason}|Reason::`string()`&#124;`binary()`|Replaces Reason header|
{event, Reason}|Event::`string()`&#124;`binary()`&#124;`nksip:token()`|Replaces Event header|
timestamp||If the request has a Timestamp_ header it is copied to the response
do100rel||Activates reliable provisional responses, if supported (must activate [plugin](../plugins/100rel.md))
{sip_etag, ETag}|ETag::`string()`&#124;`binary()`|Replaces Sip-ETag header|Only to be used in PUBLISH requests
no_dialog|-|Do not process dialogs for this request|
ignore|-|Ignore this option|


## Automatic processing

NkSIP will make some aditional processing on the response (unless you use `Code` or `{Code, Opts}`):
* If Code>100, and the request had a _Timestamp_ header, it will be copied to the response.
* If Code>100 and method is INVITE, UPDATE, SUBSCRIBE or REFER, options `allow` and `supported` will be added.
* If Code is in the 101-299 range and method is INVITE or NOTIFY, and the request had any _Record-Route_ header, they will be copied to the response.
* If Code is in the 101-299 range and method is INVITE, UPDATE, SUBSCRIBE or REFER, a `contact` option will be added if no _Contact_ is already present on the response.
* If Code is in the 200-299 range and method is REGISTER, any _Path_ header will be copied from the request to the response.
* If method is SUBSCRIBE, NOTIFY or PUBLISH, the _Event_ header will be copied from the request to the response.
* If Code is in the 200-299 range and method is SUBSCRIBE, and _Expires_ header will be added to the response if not already present.






