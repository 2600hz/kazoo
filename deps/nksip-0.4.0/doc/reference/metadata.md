# Metadata Fields
NkSIP allows the retrieval of specific information from requests, reponses, dialogs and subscriptions.
The available options are defined here for:

* [Requests and Responses](#requests-and-responses-metadata) (See [requests](../api/requests.md) and  [responses](../api/responses.md) APIs).
* [Dialogs](#fialog-metadata) (See [dialogs](../api/dialogs.md) API)
* [Subscriptions](#subscriptions-metadata) (See [subscriptions](../api/subscriptions.md) API)


Keep in mind that activated plugins can install aditional metadata fields. See the [plugins documentation](../plugins/README.md).


## Request and Responses Metadata
Accesible when calling [nksip_request:meta/2](../api/requests.md#meta2), [nksip_response:meta/2](../api/responses.md#meta2) and when using the option `meta` in the [requests sending functions](../reference/sending_options.md).

Name|Type|Description
---|---|---
handle|`nksip:handle()`|Request or response's handle
app_id|`nksip:app_id()`|Internal SipApp name this request or response belongs to
app_name|`term()`|User SipApp name this request or response belongs to
dialog_handle|`nksip:handle()`|Dialog's handle of this request or response
subscription_handle|`nksip_id()`|Subscription's handle of this request or response
proto|`nksip:protocol()`|Transport protocol
local|`{nksip:protocol(),inet:ip_address(),inet:port_number()}`|Local transport protocol, ip and port
remote|`{nksip:protocol(),inet:ip_address(),inet:port_number()}`|Remote transport protocol, ip and port
method|`nksip:method(`)|Method of the request (undefined if it is a response)
ruri|`nksip:uri()`|RUri of the request
scheme|`nksip:scheme()`|Scheme of RUri
user|`binary()`|User of RUri
domain|`binary()`|Domain of RUri
aor|`nksip:aor()`|Address-Of-Record of the RUri
code|`nksip:sip_code()`|SIP Code of the response (0 if it as request)
reason_phrase|`binary()`|Reason Phrase of the response
content_type|`nksip:token()`|Content-Type header
body|`nksip:body()`|Body
call_id|`nksip:call_id()`|Call-ID header
vias|`[nksip:via()]`|Via headers
from|`nksip:uri()`|From header
from_tag|`nksip:tag()`|From tag
from_scheme|`nksip:scheme()`|From SIP scheme
from_user|`binary()`|From user
from_domain|`binary()`|From domain
to|`nksip:uri()`|To header
to_tag|`nksip:tag()`|To tag
to_scheme|`nksip:scheme()`|To SIP scheme
to_user|`binary()`|To user
to_domain|`binary()`|To domain
cseq_num|`integer()`|CSeq (numeric part)
cseq_method|`nksip:method()`|CSeq (method part)
forwards|`integer()`|Max-Forwards header
routes|`[nksip:uri()]`|Route headers
contacts|`[nksip:uri()]`|Contact headers
require|`[binary()]`|Tokens in Require header
supported|`[binary()]`|Tokens in Supported header
expires|`integer()`&#124;`undefined`|Expires header
expired|`boolean()`|`true`if the request has expired (looking at Expires and Date headers, or received date if missing)
event|`nksip:token()`&#124;`undefined`|Token in Event header
retry_after|`integer()`&#124;`undefined`&#124;`error`|Retry-After header
refer_to|`nksip:uri()`&#124;`error`|URL in Refer-To header
realms|`[binary()]`|Realms in authentication headers
rseq_num|`integer()`&#124;`undefined`|RSeq header (numeric part)
rack|`{integer(),integer(),nksip:method()}`&#124;`undefined`|RAck header
{header, Name}|`[binary()]`|Gets an unparsed header value
all_headers|`[{binary(),[binary()]}]`|Gets all headers and values

Besides this values, you can use any `string()` or `binary()` to the get that header's value (use allways lowercase).


## Dialogs Metadata
Available when calling [nksip_dialog:meta/2](../api/dialogs.md#meta2).

Name|Type|Description
---|---|---
handle|`nksip:handle()`|Dialog's handle
app_id|`nksip:app_id()`|Internal SipApp name this dialog belongs to
app_name|`term()`|User SipApp name this dialog belongs to
created|`nksip_lib:timestamp()`|Creation date
updated|`nksip_lib:timestamp()`|Last update
local_seq|`integer()`|Local CSeq number
remote_seq|`integer()`|Remote CSeq number
local_uri|`nksip:uri()`|Local URI
raw_local_uri|`binary()`|Unparsed Local URI
remote_uri|`nksip:uri()`|Remote URI
raw_remote_uri|`binary()`|Unparsed Remote URI
local_target|`nksip:uri()`|Local Target URI
raw_local_target|`binary()`|Unparsed Local Target URI
remote_target|`nksip:uri()`|Remote Target URI
raw_remote_target|`binary()`|Unparsed Remote Target URi
early|`boolean()`|Early dialog (no final response yet)
secure|`boolean()`|Secure (sips) dialog
route_set|`nksip:uri()}`|Route Set
raw_route_set|`binary()`|Unparsed Route Set
invite_status|`nksip_dialog:status()`|Current dialog's INVITE status
invite_answered|`nksip_lib:timestamp()}`|Answer (first 2xx response) timestamp for INVITE usages
invite_local_sdp|`nksip:sdp()}`|Current local SDP
invite_remote_sdp|`nksip:sdp()}`|Current remote SDP
invite_timeout|`integer()`|Seconds to expire current state
subscriptions|`nksip:handle()`|Lists all active subscriptions
call_id|`nksip:call_id()`|Call-ID of the dialog
from_tag|`binary()`|From tag
to_tag|`binary()`|To tag


## Subscriptions Metadata
Available when calling [nksip_subscriptions:meta/2](../api/subscriptions.md#meta2).
All dialog options are available for subscriptions, and also:

Name|Type|Description
---|---|---
handle|`nksip:handle()`|Subscription's Id
status|`nksip_subscription:status()`|Subscription's current status
event|`nksip:token()`|Event header
raw_event|`binary()`|Unparsed Event header
class|`uac`&#124;`uas`|Class of the event, as a UAC or a UAS
answered|`nksip_lib:timestamp()`&#124;`undefined`|Time first NOTIFY was received
expires|`integer()`|Seconds reamaining to subscription expiration
