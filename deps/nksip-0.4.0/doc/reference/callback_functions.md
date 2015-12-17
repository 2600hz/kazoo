# Callback Functions

Each SipApp must provide a _callback module_. The functions this callback module can implement are described here. 
* See [Receiving Requests](../guide/receiving_requests.md) for an introduction. 
* The full list of reply options is available [here](sending_options.md).
* The default implementation of each callback can be reviewed in [nksip_sipapp.erl](../../src/nksip_sipapp.erl).
* Installed plugins can provide additional callbacks, or modify the default behaviour described in this document.See the [plugins documentation](../plugins/README.md).

## SIP Callbacks

Callback|Reason
---|---
[sip_get_user_pass/4](#sip_get_user_pass4)|Called to check a user password for a realm
[sip_authorize/3](#sip_authorize3)|Called for every incoming request to be authorized or not
[sip_route/5](#sip_route5)|Called to route the request
[sip_options/2](#sip_options2)|Called to process a OPTIONS request
[sip_register/2](#sip_register2)|Called to process a REGISTER request
[sip_invite/2](#sip_invite2)|Called to process a new out-of-dialog INVITE request
[sip_reinvite/2](#sip_reinvite2)|Called to process a new in-dialog INVITE request
[sip_cancel/3](#sip_cancel3)|Called when a pending INVITE request is cancelled
[sip_ack/2](#sip_ack2)|Called by NkSIP when a new valid in-dialog ACK request has to be processed locally
[sip_bye/2](#sip_bye2)|Called to process a BYE request
[sip_update/2](#sip_update2)|Called to process a UPDATE request
[sip_subscribe/2](#sip_subscribe2)|Called to process a new out-of-dialog SUBSCRIBE request
[sip_resubscribe/2](#sip_resubscribe2)|Called to process a new in-dialog SUBSCRIBE request
[sip_notify/2](#sip_notify2)|Called to process a NOTIFY request
[sip_refer/2](#sip_refer2)|Called to process a REFER request
[sip_publish/2](#sip_publish2)|Called to process a PUBLISH request
[sip_info/2](#sip_info2)|Called to process a INFO request
[sip_message/2](#sip_message2)|Called to process a MESSAGE request
[sip_dialog_update/3](#sip_dialog_update3)|Called when a dialog's state changes
[sip_session_update/3](#session_update3)|Called when a SDP session is created or updated


## Gen_server Callbacks

Callback|Reason
---|---
[init/1](#init1)|Called when the SipApp is launched using `nksip:start/4`
[terminate/2](#terminate2)|Called when the SipApp is stopped
[handle_call/3](#handle_call3)|Called when a direct call to the SipApp process is made using `nksip:call/2` or `nksip:call/3`
[handle_cast/2](#handle_cast2)|Called when a direct cast to the SipApp process is made using `nksip:cast/2`
[handle_info/2](#handle_info2)|Called when a unknown message is received at the SipApp process
[code_change/3](#code_change3)|See gen_server's documentation



## Callback List



<!-- SIP Callbacks ---------------------------------------------->


### sip_get_user_pass/4
```erlang
get_user_pass(User::binary(), Realm::binary(), Request:nksip:request(), Call::nksip:call()) ->
    true | false | Pass::binary().
```

Called to check the user password for a realm.

When a request is received containing a _Authorization_ or _Proxy-Authorization_ header, this function is called by NkSIP including the header's `User` and `Realm`, to check if the authorization data in the header corresponds to the user's password. You should reply with the user's password for this realm. NkSIP will use the password and the digest information in the header to check if it is valid, offering this information in the call to [sip_authorize/3](#sip_authorize3).

You can also reply `true` if you want to accept any request from this user without checking any password, or `false` if you don't have a password for this user or want her blocked.

If you don't want to store _clear-text_ passwords of your users, you can use [nksip_auth:make_ha1/3](../../src/nksip_auth.erl) to generate a _hash_ of the password for an user and a realm, and store this hash only, instead of the real password. Later on you can reply here with the hash instead of the real password.

If you don't define this function, NkSIP will reply with password `<<>>` if user is `anonymous`, and `false` for any other user. 


### sip_authorize/3
```erlang
authorize(AuthList, Request::nksip:request(), Call::nksip:call()) ->
    ok | forbidden | authenticate | {authenticate, Realm::binary()} |
    proxy_authenticate | {proxy_authenticate, Realm::binary()}
    when AuthList :: [dialog|register|{{digest, Realm::binary}, boolean()}].
```

Called for every incoming request to be authorized or not.

* If `ok` is replied the request is authorized and the request processing continues.
* If `forbidden`, a 403 _Forbidden_ is replied statelessly.
* If `authenticate` is replied, the request will be rejected (statelessly) with a 401 _Unauthorized_. The other party will usually send the request again, this time with an _Authorization_ header.
* If you reply `proxy_authenticate`, it is rejected with a 407 _Proxy Authentication Rejected_ response and the other party will include a _Proxy-Authorization_ header.

You can use the tags included in `AuthList` in order to decide to authenticate or not the request. `AuthList` includes the following tags:
* `dialog`: the request is in-dialog and coming from the same ip and port than the last request for an existing dialog.
* `register`: the request comes from the same ip, port and transport of a currently valid registration (and the method is not _REGISTER_). This option will also appear if the [nksip_registrar](../plugins/registrar.md) plugin is activated.
* `{{digest, Realm}, true}`: there is at least one valid user authenticated (has a correct password) with this `Realm`.
* `{{digest, Realm}, false}`: there is at least one user offering an authentication header for this `Realm`, but all of them have failed the authentication (no password was valid). 

You will usually want to combine these strategies. Typically you will first check using SIP digest authentication, and, in case of faillure, you can use previous registration and/or dialog authentication. 

If you don't define this function all requests will be authenticated.

Example:
```erlang
sip_authorize(Auth, Req, _Call) ->
    IsDialog = lists:member(dialog, Auth),
    IsRegister = lists:member(register, Auth),
    case IsDialog orelse IsRegister of
        true ->
            ok;
        false ->
            case nksip_lib:get_value({digest, <<"my_realm">>}, Auth) of
                true -> ok;
                false -> forbidden;
                undefined -> {proxy_authenticate, BinId}
            end
    end.
```


### sip_route/5
```erlang
sip_route(Scheme::nksip:scheme(), User::binary(), Domain::binary(), 
                Request::nksip:request(), Call::nksip:call()) ->
    proxy | {proxy, ruri | nksip:uri_set()} | 
    {proxy, ruri | nksip:uri_set(), nksip:optslist()} | 
    proxy_stateless | {proxy_stateless, ruri | nksip:uri_set()} | 
    {proxy_stateless, ruri | nksip:uri_set(), nksip:optslist()} | 
    process | process_stateless |
    {reply, nksip:sipreply()} | {reply_stateless, nksip:sipreply()}.
```

This function is called by NkSIP for every new request, to check if it must be proxied, processed locally or replied immediately. 
For convenience, the scheme, user and domain parts of the _Request-Uri_ are included.

If we want to **act as a proxy** and route the request, and we are not responsible for `Domain`, we must return `proxy` or `{proxy, ruri, ProxyOpts}`. We must not return an `UriSet` in this case. NkSIP will then make additional checks to the request (like inspecting the `Proxy-Require` header) and will route it statefully to the same `Request-URI` contained in the request.

If we are the responsible proxy for `Domain` we can provide a new list of URIs to route the request to. NkSIP will use **_serial_** and/or **_parallel_** _forking_ depending on the format of `UriSet`. If `UriSet` is a simple Erlang array of binaries representing uris, NkSIP will try each one serially. If any of the elements of the arrary is in turn a new array of binaries, it will fork them in parallel. 
For example, for  `[ <<"sip:aaa">>, [<<"sip:bbb">>, <<"sip:ccc">>], <<"sip:ddd">>]` NkSIP will first forward the request to `aaa`. If it does not receive a successful (2xx) response, it will try `bbb` and `cccc` in parallel. If no 2xx is received again, `ddd` will be tried. See `nksip_registrar` to find out how to get the registered contacts for this _Request-Uri_. For `ProxyOpts` you can use the same options defined for [sending requests](sending_options.md). Common options are:

* `record_route`: NkSIP will insert a _Record-Route_ header before sending the request, so that following request inside the dialog will be routed to this proxy.
* `path`: For REGISTER requests, if the request includes "path" as part of the supported tokens, it will insert a _Path_ header (see RFC3327). If path it is not supported, it will reply a 421 _Extension Required_ response.
* `follow_redirects`: If any 3xx response is received, the received contacts will be inserted in the list of uris to try.
* `{insert, "route", Routes}`: NkSIP will insert theses routes as _Route_ headers in the request, before any other existing `Route` header. The request would then be sent to the first _Route_.

You can also add headers to the request if the URI contains a _Route_ header.

If you use `proxy_stataless` instead of `proxy`, the request will be proxied statelessly, without storing any state about it in the server. In this case you should use _only one url_.

If we want to **act as an endpoint or B2BUA** and answer to the request from this SipApp, we must return `process` or `process_stataless`. NkSIP will then make additional checks to the request (like inspecting `Require` header), and will call the function corresponding to the method in the request (like [sip_invite/2](#sip_invite2), [sip_options/2](#sip_options2), etc.)

We can also **send a reply immediately**, replying `{reply, Reply}` or `{reply_stateless, Reply}`. See [the reply options](reply_options.md) for a descriptions of available reply options.

If `route/5` is not defined the default reply would be `process`.

Example:
```erlang
sip_route(Scheme, User, Domain, Req, _Call) ->
    Opts = [{route, "<sip:127.0.0.1:5061;lr>"}],
    case User of
        <<>> -> 
            process;
        _ when Domain =:= <<"127.0.0.1">> ->
            proxy;
        _ ->
            {ok, App} = nksip_request:app_name(Req),
            case nksip_registrar:find(App, Scheme, User, Domain) of
                [] -> 
                    {reply, temporarily_unavailable};
                UriList -> 
                    {proxy, UriList, Opts}
            end
    end.
```


### sip_options/2
```erlang
sip_options(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

Called when a OPTIONS request is received.

This function is called by NkSIP to process a new incoming OPTIONS request as an endpoint. If not defined, NkSIP will reply with a _200 OK_ response, including options `contact`, `allow`, `allow_event`, `accept` and `supported`. See the list of available options [here](reply_options.md).

NkSIP will not send any body in its automatic response. This is ok for proxies. If you are implementing an endpoint or B2BUA, you should implement this function and include in your response a SDP body representing your supported list of codecs, and also the previous options.


### sip_register/2
```erlang
sip_register(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new incoming REGISTER request. 

If the you don't implement this function, and the [nksip_registrar](../plugins/registrar.md) plugin is activated, it will be processed as a registrar server. It will NOT check if _From_ and _To_ headers contains the same URI, or if the registered domain is valid or not. If you need to check this, implement this function, and reply the response from calling `nksip_registrar:request/1` if everything is ok. 

If this function is not defined, and the registrar plugin is not activated, a _405 Method not allowed_ would be replied. 

```erlang
register(ReqId::nksip:handle(), Meta::meta(), From::from(), State::term()) ->
    call_reply(nksip:sipreply()).

register(Req, Call) ->
    {ok, [{from_scheme, FromScheme}, {from_user, FromUser}, {from_domain, FromDomain}]} = 
        nksip_request:metas([from_scheme, from_user, from_domain], Req),
    {ok, [{to_scheme, ToScheme}, {to_user, ToUser}, {to_domain, ToDomain}]} = 
        nksip_request:metas([to_scheme, to_user, to_domain], Req),
    case {FromScheme, FromUser, FromDomain} of
        {ToScheme, ToUser, ToDomain} ->
            {reply, nksip_registrar:request(Request)};
        _ ->
            {reply, forbidden}
    end.
```


### sip_invite/2
```erlang
sip_invite(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new INVITE request as an endpoint.

You would usually extract information from the Request (using the [Request API](../api/requests.md)) like user, body, content-type, etc., and maybe get information from the body with the functions in the [SDP API](../api/sdp.md), and you should then reply a final response (like `ok`, `{answer, Body}` or `busy`, see the [reply options](reply_options.md)).

Alternatively, you can grab a _request handle_ (calling `nksip_request:get_handle(Request)`), return a provisional (1xx) response (like `{reply, ringing}`) and _spawn_ a new process to process the request and send a final response, later on, calling `nksip_request:reply/2`. You should use this technique also if you are going to spend more than a few miliseconds processing the callback function, in order to not to block new requests and retransmissions having the same _Call-ID_. You shouldn't copy the `Request` and `Call` objects to the spawned process, as they are quite heavy. It is recommended to extract all needed information before spawning the request and pass it to the spawned process.

NkSIP will usually send a _100 Trying_ response before calling this callback, unless option `no_100` is used.

Inmediatly after you send the first provisional or final response, a dialog will be created, and NkSIP will call [sip_dialog_update/3](#sip_dialog_update3) and possibly [sip_session_update/3](#sip_session_update3). The dialog will be destroyed if no ACK in received after sending a successful final response.

To generate _reliable provisional responses_, activate the [nksip_100rel](../plugins/100rel.md) plugin and reply `rel_ringing` or `rel_session_progress`.

To use session timers, activate the  [nksip_timers](../plugins/timers.md) plugin.


Example:
```erlang
sip_invite(Req, Call) ->
    {ok, ReqId} = nksip_request:get_handle(Req),
    HasSDP = case nksip_dialog:get_dialog(Req, Call) of
        {ok, Dialog} -> {true, nksip_dialog:meta(invite_local_sdp, Dialog)};
        {error, _} -> false
    end,
    proc_lib:spawn(
        fun() ->
            nksip_request:reply(ringing, ReqId),
            timer:sleep(5000),
            case HasSDP of
                {true, SDP1} ->
                    SDP2 = nksip_sdp:increment(SDP1),
                    nksip_request:reply({ok, [{body, SDP2}|Hds]}, ReqId);
                false ->
                    nksip_request:reply(decline, ReqId)
            end
        end),
    noreply.
```

If this functions is not implemented, it will reply with a _603 Decline_.


### sip_reinvite/2
```erlang
sip_reinvite(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called when a new in-dialog INVITE request is received. 

It works the same way as [sip_invite/2](#sip_invite2), and, if it is not implement, it will simply call it. You should implement it to have different behaviours for new and in-dialog INVITE requests.


### sip_cancel/3
```erlang
sip_cancel(InviteRequest::nksip:request(), Request::nksip:request(), 
                 Call::nksip:call()) ->
    ok.
```

Called when a pending INVITE request is cancelled.

When a CANCEL request is received by NkSIP, it will check if it belongs to an existing INVITE transaction. If not, a 481 _Call/Transaction does not exist_ will be automatically replied. If it belongs to an existing INVITE transaction, NkSIP replies 200 _OK_ to the CANCEL request. If the matching INVITE transaction has not yet replied a final response, NkSIP replies it with a _487 Request Terminated_ and this function is called. If a final response has already beeing replied, it has no effect.

You can get additional information of the cancelled INVITE using `InviteRequest` with the [Request API](../api/requests.md).


### sip_ack/2
```erlang
sip_ack(Request::nksip:request(), Call::nksip:call()) ->
    ok.
```

This function is called by NkSIP when a new valid in-dialog ACK request has to be processed locally.

You don't usually need to implement this callback. One possible reason to do it is to receive the SDP body from the other party in case it was not present in the INVITE (you can also get it from the [sip_session_update/3](#sip_session_update3) callback).


### sip_bye/2
```erlang
sip_bye(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

Called when a valid BYE request is received.

When a BYE request is received, NkSIP will automatically response 481 _Call/Transaction does not exist_ if it doesn't belong to a current dialog. If it does, NkSIP stops the dialog and this callback functions is called. You won't usually need to implement this function, but in case you do, you should reply `ok` to send a _200 OK_ response back. 



### sip_update/2
```erlang
sip_update(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

Called when a valid UPDATE request is received.

When a UPDATE request is received, NkSIP will automatically response _481 Call/Transaction does not exist_ if it doesn't belong to a current dialog. If it does, this function is called. The request will probably have a SDP body. If an `{answer, Body}` is replied with an SDP body, the session may change (and the corresponding callback function [sip_session_update/3)(#sip_session_update4) will be called). If other non 2xx response is replied (like decline) the media is not changed.

If not implemented will reply _603 Decline_.


### sip_subscribe/2
```erlang
sip_subscribe(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new incoming SUBSCRIBE request that has an allowed _Event_ header.

If you reply a 2xx response like `ok` or `accepted`, a dialog and a subscription will start, and you **must inmeditaly send a NOTIFY** using [nksip_uac:notify/3](sending_functions.md#notify). You can use the option `{expires, Seconds}` to override the expires present in the request, but the new value must be lower, or 0 to cancel the subscription.

Example:
```erlang
subscribe(Req, Call) ->
    case nksip_request:meta(event, Req) of
        {ok, {<<"my_event">>, _}} -> {reply, {ok, [{expires, 10}]}};
        _ -> {reply, forbidden}
    end.
```

If not implemented will reply _603 Decline_.


### sip_resubscribe/2
```erlang
sip_resubscribe(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new in-subscription SUBSCRIBE request, sent in order to refresh the subscription.

You won't usually have to implement this function, if not implemented it will reply _200 OK_ and the subscription will be refreshed.


### sip_notify/2
```erlang
sip_notify(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new incoming NOTIFY request belonging to a current active subscription.

You should extract any relevant information and return `ok`. If not implemented, NkSIP will reply _200 OK_.



### sip_refer/2
```erlang
sip_refer(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new incoming REFER.

If you want to be able to process incoming REFERs in a simple way, use the [nksip_refer](../plugins/refer.md) plugin. 

If you want to process it manually, you should send `ok` if the request has been accepte or `decline` if not. If you are going to spend more than a few miliseconds to reply, you should reply `accepted`, and if the request is not accepted later on, send a NOTIFY with appropiate reason.

If you reply a 2xx response like `ok`  or `accepted`, a dialog and a subscription will start. You should extract the _Refer-To_ header, start a new INVITE to that url and, according to RFC3515,  send any response back to the subscription using [nksip_uac:notify/3](sending_functions.md#notify). The request sending functions accept option `{refer_subscription_id, SubsHandle}` to automatically send a valid NOTIFY after each provisional or the final response.

This would be a typical implementation:
```erlang
refer(Req, Call) ->
    case nksip_request:meta(refer_to, Req) of
        {ok, Uri} ->
            {ok, SubsId} = nksip_subscription:get_handle(Req), 
            {ok, AppId} = nksip_request:app_id(Req),
            Opts = [async, auto_2xx_ack, {refer_subscription_id, SubsId}],
            spawn(fun() -> nksip_uac:invite(AppId, ReferTo, Opts) end),
            {reply, ok};
        error ->
            {reply, invalid_request}
    end.
```

If not implemented a _603 Decline_ will be returned.


### sip_publish/2
```erlang
sip_publish(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

This function is called by NkSIP to process a new incoming PUBLISH request. 

If you want to use NkSIP's _event state compositor_, see the [nksip_event_compositor](../plugins/event_compositor.md) plugin that manages everything automatically.

If not implemented a _603 Decline_ will be returned.


### sip_info/2
```erlang
sip_info(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```

Called when a valid INFO request is received.

When an INFO request is received, NkSIP will automatically response 481 _Call/Transaction does not exist_ if it doesn't belong to a current dialog. If it does, NkSIP this callback functions is called. If implementing this function, you should reply `ok` to send a _200 OK_ response back.


### sip_message/2
```erlang
sip_message(Request::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.
```
This function is called by NkSIP to process a new incoming MESSAGE request.

If you reply a 2xx response like `ok` or `accepted`, you are telling to the remote party that the message has been received. Use a 6xx response (like `decline`) to tell that it has been refused.
If not implemented a _603 Decline_ will be returned.


### sip_dialog_update/3
```erlang
sip_dialog_update(DialogStatus, Dialog::nksip:dialog(), Call::nksip:call()) ->
    ok
    when DialogStatus :: start | target_update | 
                         {invite_status, nksip_dialog:invite_status()} |
                         {subscription_status, nksip_subscription:status(), nksip:subscription()} |
                         {stop, nksip_dialog:stop_reason()}.Called when a dialog has changed its state.
```

A new dialog will be created when you send a new dialog-forming request (like INVITE, SUBSCRIBE or REFER) and a provisional (1xx) or successful (2xx) response is received. If the response is provisional, the dialog will be marked as temporary or _early_, waiting for the final response to be confirmed or deleted. Dialogs will also be created for _subscriptions_, after a valid NOTIFY is sent or received. 

Once the dialog is established, some in-dialog methods (like INVITE, UPDATE, SUBSCRIBE and NOTIFY) can update the `target` of the dialog. 

Any dialog can have multiple usages simultaneously, as much as _one_ _INVITE usage_ and a _unlimited_ number of _SUBSCRIBE usages_. The _INVITE usage_ is destroyed when a valid in-dialog BYE request is sent or received. A _SUBSCRIPTION usage_ is destroyed when a NOTIFY with _status=terminated_ is received. When no usage is left, the dialog itself is destroyed.

NkSIP will call this function every time a dialog is created, its target is updated or it is destroyed. It will be called also when the status of the _usage_ changes, as `{invite_status, Status}` for the INVITE usage and `{subscription_status, Status, Subscription}` for SUBSCRIBE usages.

```erlang
dialog_update(DialogId::nksip:handle(), DialogStatus, State::term()) ->
    ok
    when DialogStatus :: start | target_update | 
                         {invite_status, nksip_dialog:invite_status()} |
                         {subscription_status, nksip_subscription:status(), nksip:subscription()} |
                         {stop, nksip_dialog:stop_reason()}.
```


### sip_session_update/3
```erlang
sip_session_update(SessionStatus, Dialog::nksip:dialog(), Call::nksip:call()) ->
    ok 
    when SessionStatus :: {start, Local, Remote} | {update, Local, Remote} | stop,
                          Local::nksip_sdp:sdp(), Remote::nksip_sdp:sdp().
```

Called when a dialog has updated its SDP session parameters.

When NkSIP will call this function when detects that, inside an existing dialog, both parties have agreed on a specific SDP defined session. You can use the functions in [SDP API](../api/sdp.md) to process the SDP data. This function will be also called after each new successful SDP negotiation.



<!-- gen_server Callbacks ---------------------------------------------->



### init/1
This callback function is called when the SipApp is launched using `nksip:start/4`.
If `{ok, State}` or `{ok, State, Timeout}` is returned the SipApp is started with this initial state. If a `Timeout` is provided (in milliseconds) a `timeout` message will be sent to the process (you will need to implement `handle_info/2` to receive it). If `{stop, Reason}` is returned the SipApp will not start. 

```erlang
init(Args::term()) ->
    {ok, State::term()} | {ok, State::term(), Timeout::timeout()} |
    {stop, Reason::term()}.

init([]) ->
    {ok, {}}.
```

### terminate/2
Called when the SipApp is stopped.

```erlang
terminate(Reason::term(), State::term()) ->
    ok.

terminate(_Reason, _State) ->
    ok.
```

### handle_call/3
Called when a direct call to the SipApp process is made using `gen_server:call/2,3`.

```erlang
handle_call(Msg::term(), From::from(), State::term()) ->
      {noreply, State} | {noreply, State, Timeout} | 
      {reply, Reply, State} | {reply, Reply, State, Timeout} | 
      {stop, Reason, State} | {stop, Reason, Reply, State}
      when State :: term(), Timeout :: infinity | non_neg_integer(), Reason :: term().

handle_call(Msg, _From, State) ->
    lager:warning("Unexpected handle_call in ~p: ~p", [Msg, ?MODULE]),
    {noreply, State}.
```


### handle_cast/2
Called when a direct cast to the SipApp process is made using `gen_server:cast/2`.

```erlang
handle_cast(Msg::term(), State::term()) ->
      {noreply, State} | {noreply, State, Timeout} | 
      {stop, Reason, State} 
      when State :: term(), Timeout :: infinity | non_neg_integer(), Reason :: term().

handle_cast(Msg, State) ->
    lager:warning("Unexpected handle_cast in ~p: ~p", [Msg, ?MODULE]),
    {noreply, State}.
```


### handle_info/2
Called when the SipApp process receives an unknown message.

```erlang
handle_info(Msg::term(), State::term()) ->
      {noreply, State} | {noreply, State, Timeout} | 
      {stop, Reason, State} 
      when State :: term(), Timeout :: infinity | non_neg_integer(), Reason :: term().

handle_info(_Msg, State) ->
    {noreply, State}.
```

### code_change/3
See gen_server's documentation


```erlang
code_change(OldVsn::term(), State::term(), Extra::term()) ->
    {ok, NewState::term()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```
