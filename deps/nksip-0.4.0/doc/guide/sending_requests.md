# Sending Requests

To send a new request, you should use [one of the functions](../reference/sending_functions.md) in [nksip_uac](../../src/nksip_uac.erl) module. NkSIP supports all defined SIP methods: OPTIONS, REGISTER, INVITE, ACK, BYE, CANCEL, INFO, UPDATE, SUBSCRIBE, NOTIFY, REFER, PUBLISH and MESSAGE (PRACKs are sent automatically by the [nksip_100rel](../plugins/100rel.md) plugin).

Depending on the specific method, the request should be sent _out of any existing dialog_ or _in-dialog_. Out-of-dialog sending request functions will need a SipApp name (_user name_ or _internal name_), a _sip uri_ to send the request to and an optional [list of options](../reference/sending_options.md). In-dialog sending request functions will usually need the _dialog's id_ or _subscription's id_ of the dialog or subscription. You can get them from the returned _meta_ values of dialog-generating functions (like INVITE or SUBSCRIBE) or from [sip_dialog_update/3](../reference/callback_functions.md#sip_dialog_update3) callback function. 

The returned value can include some metadata about the response. Use the option `meta` to select which metadatas you want to receive (see [metadata fields](../reference/metadata.md)). Some methods (like INVITE and SUBSCRIBE) will allways include some metadata (see bellow). You can use the [Dialog API functions](../api/dialogs.md) to get additional information.

You can define a callback function using option `callback`, and it will be called for every received provisional response as `{reply, Code, Response, Call}`. Use the functions in [Responses API](../api/responses.md) to extract relevant information from each specific response. It's important to notice this callback function will be called in the same process as the call (see explanation in [callback functions](../reference/callback_functions.md)), so you shouldn't spend a lot of time inside it. You shouldn't also copy the `Response` or `Call` object to other processes, because they are quite heavy.

By default, most functions will block until a final response is received or a an error is produced before sending the request, returning `{ok, Code, Meta}` or `{error, Error}`. You can also call most of these functions _asynchronously_ using `async` option, and the call will return immediately with `{async, RequestId}` before even trying to send the request, instead of blocking. You should use the callback function to receive provisional responses, final response and errors. `RequestId` is a handle to the current request and can be used to get additional information from it before it's destroyed (see, the [Requests API](../api/requests.md)) or to CANCEL the request.

Most functions in this list add specific behaviour for each specific method. For example, `invite/3,2` will allways include a Contact header. You can use the generic `request/3,2` function to avoid any specific addition. 

In case of using a SIP URI as destination, is is possible to include custom headers, for example `<sip:host;method=REGISTER?contact=*&expires=10>`, but it must be escaped (using for example `http_uri:encode/1`). You should use `request/3,2` if you specify the method in the _uri_.

See the fill list of sending functions in [Sending Functions](../reference/sending_functions.md), and the full list of options in [Sending Options](../reference/sending_options.md).
