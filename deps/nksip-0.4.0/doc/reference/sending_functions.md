# Request Sending Functions

See a general description [Sending Requests](../guide/sending_requests.md) and and the full list of options in [Sending Options](../reference/sending_options.md).

All these functions are defined in [nksip_uac.erl](../../src/nksip_uac.erl)


Function|Comment
---|---
[options/3](#options)|Sends an out-of-dialog OPTIONS request
[options/2](#options)|Sends an in-dialog OPTIONS request
[register/3](#register)|Sends an out-of-dialog REGISTER request
[invite/3](#invite)|Sends an out-of-dialog INVITE request
[invite/2](#invite)|Sends an in-dialog INVITE request
[ack/2](#ack)|Sends an in-dialog ACK request for a successful INVITE response
[bye/2](#bye)|Sends an in-dialog BYE request
[cancel/2](#cancel)|Sends a CANCEL for a previous sent INVITE
[update/2](#update)|Sends an in-dialog UPDATE request
[info/2](#info)|Sends an in-dialog INFO request
[subscribe/3](#subscribe)|Sends an out-of-dialog SUBSCRIBE request
[subscribe/2](#subscribe)|Sends an in-dialog or in-subscription SUBSCRIBE request
[notify/2](#notify)|Sends an in-dialog NOTIFY request
[message/3](#message)|Sends an out-of-dialog MESSAGE request
[message/2](#message)|Sends an in-dialog MESSAGE request
[refer/3](#refer)|Sends an out-of-dialog REFER request
[refer/2](#refer)|Sends an in-dialog REFER request
[publish/3](#publish)|Sends an out-of-dialog PUBLISH request
[publish/2](#publish)|Sends an in-dialog PUBLISH request
[request/3](#generic-request)|Sends an out-of-dialog generic request
[request/2](#generic-request)|Sends an in-dialog generic request
[stun/3](#stun)|Sends a STUN request


## Functions Description

### options
```
nksip_uac:options(App, Uri, Opts)
nksip_uac:options(DialogId, Opts)
```

OPTIONS requests are usually sent to get the current set of SIP features and codecs the remote party supports, and to detect if it is _up_, it has failed or it is not responding requests for any reason. It can also be used to measure the remote party response time.

Options `supported`, `allow` and `allow_event` are automatically added.

NkSIP has an automatic remote _pinging_ feature that can be activated on any SipApp, using [nksip_uac_auto_register](../plugins/auto_register.md) plugin.


### register
```
nksip_uac:register(App, Uri, Opts)
```

This function is used to send a new REGISTER request to any registrar server, to register a new _Contact_, delete a current registration or get the list of current registered contacts from the registrar. To register a contact you should use optons `{contact, Contact}` or `contact`, and typically `expires`. If you include no contact, the current list of registered contacts should be returned by the server (use `contact` as _meta_ option to get it)

Options `to_as_from`, `supported`, `allow` and `allow_events` are automatically added. 
You can use also use the options `unregister` to unregister included or default contact and `unregister_all` to unregister all contacts. 

NkSIP has an automatic registration feature that can be activated using [nksip_uac_auto_register](../plugins/auto_register.md) plugin. For outbound compatible registrations, see [nksip_uac_auto_outbound](../plugins/auto_outbound.md) plugin.

Keep in mind that, once you send a REGISTER requests, following refreshes should have the same _Call-ID_ and incremented _CSeq_ headers.



### invite
```
nksip_uac:invite(App, Uri, Opts)
nksip_uac:invite(DialogId, Opts)
```

These functions sends a new session invitation to another endpoint or proxy. Options `contact`, `supported`, `allow` and `allow_event` are automatically added.

When the first provisional response from the remote party is received (as 180 _Ringing_) a new dialog will be started, and the corresponding callback [sip_dialog_update/3](../reference/callback_functions.md#sip_dialog_update3) in the callback module will be called. If this response has also a valid SDP body, a new session will be associated with the dialog and the corresponding callback [sip_session_update/3](../reference/callback_functions.md#sip_session_update3)  will also be called.

When the first 2xx response is received, the dialog is confirmed. **You must then call `ack/2` immediately, or use the `auto_2xx_ack` option**, offering an SDP body if you haven't done it in the INVITE request. The dialog is destroyed when a BYE is sent or received, or a 408 _Timeout_ or 481 _Call Does Not Exist_ response is received. If a secondary 2xx response is received (usually because a proxy server has forked the request) NkSIP will automatically acknowledge it and send BYE. 

If a 3xx-6xx response is received instead of a 2xx response, the _early dialog_ is destroyed. You should not call `ack/2` in this case, as NkSIP will do it for you automatically.

After a dialog has being established, you can send new INVITE requests (called _reINVITEs_) _inside_ this dialog, as well as in-dialog OPTIONS, BYE, UPDATE, INFO, SUBSCRIBE, MESSAGE, REFER or PUBLISH. If no request is sent or received in the dialog after a time (defined in the `dialog_timeout` configuration option), it is destroyed.

If you activate the [session timers plugin](../plugins/timers.md) NkSIP will have a different timing behaviours.

If you use the option `{expires, Expires}`, NkSIP will CANCEL the request if no final response has been received in this period in seconds. 

If you want to be able to _CANCEL_ the request, you should use the `async` option to get the corresponding `RequestId` to use when calling `cancel/2`.

If a 491 response is received, it usually means that the remote party is starting another reINVITE transaction right now. You should call `nksip_response:wait_491/0` and try again.

For successful (2xx) responses, the first _meta_ returned value is allways `{dialog, DialogHandle}`, even if the `meta` option is not used.




### ack
```
nksip_uac:ack(DialogId, Opts)
```

After sending an INVITE and receiving a successfully (2xx) response, you must call this function immediately to send the mandatory ACK (unless option `auto_2xx_ack` is used). NkSIP won't send it for you automatically in case of a successful response, because you may want to include a SDP body if you didn`t do it in the INVITE request.

For _sync_ requests, it will return `ok` if the request could be sent or `{error, Error}` if an error is detected. For _async_ requests, it will return `async`. If a callback is defined, it will be called as `ok` or `{error, Error}`.    


### bye
```
nksip_uac:bye(DialogId, Opts)
```

Sends an _BYE_ for a current dialog, terminating the session.


### cancel

```
nksip_uac:cancel(RequestId, Opts)
```

Sends an _CANCEL_ for a currently ongoing _INVITE_ request.

You can use this function to send a CANCEL requests to abort a currently _calling_ INVITE request, using the `RequestId` obtained when calling `invite/2,3` _asynchronously_. The CANCEL request will eventually be received at the remote end, and, if it hasn't yet answered the matching INVITE request, it will finish it with a 487 code. 

The only recognized option is `{reason, string()|binary()}`.

This call is always asychronous. It returns a soon as the request is received and the cancelling INVITE is found.


### update

```
nksip_uac:update(DialogId, Opts)
```

Sends an  UPDATE on a currently ongoing dialog, allowing to change the media session before the dialog has been confirmed. A session timer will be started if the [session timers plugin](../plugins/timers.md) is activated.
Options `supported`, `accept` and `allow` are automatically added.


### info

```
nksip_uac:info(DialogId, Opts)
```

Sends an INFO request. Doesn`t change the state of the current session.



### subscribe
```
nksip_uac:subscribe(App, Uri, Opts)
nksip_uac:subscribe(Id, Opts)
```

Sends an SUBSCRIBE request.

These functions send a new subscription request to the other party. You **must** use option `{event, Event}` to select an _Event Package_ supported at the server, and commonly an `{expires, Expires}` option (default for this package will be used if expires is not defined). Options `contact`, `supported`, `allow` and `allow_event` are automatically added.

If the remote party returns a 2xx response, it means that the subscription has been accepted, and a NOTIFY request should arrive inmediatly. After the reception of the NOTIFY, NkSIP will call the corresponding callback [sip_notify/2](../reference/callback_functions.md#sip_notify2) and the subscription state will change, so NkSIP will call [sip_dialog_update/3](../reference/callback_functions.md#sip_dialog_update3).

If `Id` is a _subscription's id_, it will send as a reSUBSCRIBE, using the same _Event_ and _Expires_ as the last _SUBSCRIBE_, refreshing the subscription in order to avoid its expiration.

After a 2xx response, you should send a new reSUBSCRIBE request to refresh the subscription before the indicated _Expires_, calling this function again but using the subscription specification. When half the time before expire has been completed, NkSIP will call callback [sip_dialog_update/3](../reference/callback_functions.md#sip_dialog_update3) as `{subscription_state, middle_timer, Subscription}` to remind you to send the reSUBSCRIBE.


### notify
```
nksip_uac:notify(SubscriptionId, Opts)
```

Sends an _NOTIFY_ for a current server subscription.

When your SipApp accepts a incoming SUBSCRIBE request, replying a 2xx response, you should send a NOTIFY inmediatly. You have to use the subscription's id from the call to callback `subscribe/3`. NkSIP will include the mandatory _Event_ and _Subscription-State_ headers for you, but **you must include** a `{subscription_state, ST}` option with the following allowed values:

* `active`: the subscription is active. NkSIP will add a `expires` parameter indicating the remaining time.
* `pending`: the subscription has not yet been authorized. A `expires` parameter will be added.
* `{terminated, Reason}`: the subscription has been terminated. You must use a reason: `deactivated` (the remote party should retry again inmediatly), `probation` (the remote party should retry again), `rejected` (the remote party should no retry again), `timeout` (the subscription has timed out, the remote party can send a new one inmediatly), `giveup` (we have not been able to authorize the request, the remote party can try again), `noresource` (the subscription has ended because of the resource does not exists any more, do not retry) or `invariant` (the subscription has ended because of the resource is not going to change soon).
* `{terminated, Reason, Retry}`: Only with reasons `probation` and `giveup` you can send a retry-after parameter.


### message
```
nksip_uac:message(App, Uri, Opts)
nksip_uac:message(DialogId, Opts
```

Sends an MESSAGE request.


### refer
```
nksip_uac:refer(App, Uri, Opts)
nksip_uac:refer(DialogId, Opts
```

Sends an _REFER_ for a remote party. 

Asks the remote party to start a new connection to the indicated uri in the mandatory `refer_to` parameter. If a 2xx response is received, the remote party has agreed and will start a new connection. A new subscription will be stablished, and you will start to receive NOTIFYs. Implement the callback function [sip_notify/2](../reference/callback_functions.md#sip_notify2) to receive them, filtering using the indicated `subscription`.

In case of 2xx response, the first returned value is allways `{subscription, SubscriptionHandle}`, even if the `meta` option is not used.

If you activate the [nksip_refer plugin](../plugins/refer.md), NkSIP processes this information automatically.


### publish 

```
nksip_uac:publish(App, Uri, Opts)
nksip_uac:publish(DialogId, Opts)
```

Sends an PUBLISH request. 

This functions sends a new publishing to the other party, you **must** include the mandatory `{event, Event}` remote supported event package and include a body. Options `supported`, `allow` and `allow_event` are automatically added.

If the remote party returns a 2xx response, it means that the publishing has been accepted, and the body has been stored. A _SIP-ETag_ header will be returned (a `sip_etag` parameter will always be returned in meta). You can use this ETag (using `{sip_if_match, ETag}` option) to update the stored information (sending a new body), or deleting it (using `{expires, 0}`).

NkSIP includes the implementation of an Event State Compositor (the server that must receive the PUBLISH requests) in the [nksip_event_compositor](../plugins/event_compositor.md) plugin.



### Generic Request
```
nksip_uac:request(App, Uri, Opts)
nksip_uac:request(DialogId, Opts)
```

Allows you to send any SIP request, without the automatic processing of the previous functions. 


### stun
```
nksip_uac:stun(App, Uri, Opts)
```

Sends a new _STUN_ binding request.

Use this function to send a STUN binding request to a remote STUN or STUN-enabled SIP server, in order to get our remote ip address and port. If the remote server is a standard STUN server, use port 3478 (i.e. `sip:stunserver.org:3478`). If it is a STUN server embedded into a SIP UDP, use a standard SIP uri.


